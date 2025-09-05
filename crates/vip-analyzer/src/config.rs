//! Config used by the language server.
//!
//! Of particular interest is the `feature_flags` hash map: while other fields
//! configure the server itself, feature flags are passed into analysis, and
//! tweak things like automatic insertion of `()` in completions.
use std::{fmt, iter, sync::OnceLock};

use itertools::Itertools;
use paths::Utf8PathBuf;
use project_model::{IdeVariables, ManifestPath, ManifestVars};
use rustc_hash::FxHashMap;
use semver::Version;
use serde::{Deserialize, Serialize, de::DeserializeOwned};
use stdx::format_to_acc;
use toolchain::{BuildAction, VipBuilderOptions};
use triomphe::Arc;
use vfs::{AbsPath, AbsPathBuf};

use crate::{
    flycheck::FlycheckConfig,
    lsp::capabilities::ClientCapabilities,
    lsp_ext::{WorkspaceSymbolSearchKind, WorkspaceSymbolSearchScope},
};

// Conventions for configuration keys to preserve maximal extendability without breakage:
//  - Toggles (be it binary true/false or with more options in-between) should almost always suffix as `_enable`
//    This has the benefit of namespaces being extensible, and if the suffix doesn't fit later it can be changed without breakage.
//  - In general be wary of using the namespace of something verbatim, it prevents us from adding subkeys in the future
//  - Don't use abbreviations unless really necessary
//  - foo_command = overrides the subcommand, foo_overrideCommand allows full overwriting, extra args only applies for foo_command

config_data! {
    /// Configs that only make sense when they are set by a client. As such they can only be defined
    /// by setting them using client's settings (e.g `settings.json` on VS Code).
    client: struct ClientDefaultConfigData <- ClientConfigInput -> {
        /// Warm up caches on project load.
        cachePriming_enable: bool = true,
        /// How many worker threads to handle priming caches. The default `0` means to pick automatically.
        cachePriming_numThreads: NumThreads = NumThreads::Physical,

        /// Run the check command for diagnostics on save.
        checkOnSave : bool = true,

        /// How many worker threads in the main loop. The default `null` means to pick automatically.
        numThreads: Option<NumThreads> = None,

        /// Path variables available per VIP project.
        workspace_ideVariables: FxHashMap<ManifestPath, ManifestVars> = FxHashMap::default(),
    }
}

#[derive(Clone, Debug)]
struct ClientInfo {
    name: String,
    version: Option<Version>,
}

#[derive(Debug, Clone)]
pub struct Config {
    pub(crate) discovered_projects: Vec<ManifestPath>,
    /// The workspace roots as registered by the LSP client
    workspace_roots: Vec<AbsPathBuf>,
    caps: ClientCapabilities,
    root_path: AbsPathBuf,
    client_info: Option<ClientInfo>,

    default_config: &'static ClientDefaultConfigData,
    /// Config node that obtains its initial value during the server initialization and
    /// by receiving a `lsp_types::notification::DidChangeConfiguration`.
    client_config: (ClientConfigInput, ConfigErrors),

    /// Use case : It is an error to have an empty value for `check_command`.
    /// Since it is a `global` command at the moment, its final value can only be determined by
    /// traversing through `global` configs and the `client` config. However the non-null value constraint
    /// is config level agnostic, so this requires an independent error storage
    validation_errors: ConfigErrors,
}

// Delegate capability fetching methods
impl std::ops::Deref for Config {
    type Target = ClientCapabilities;

    fn deref(&self) -> &Self::Target {
        &self.caps
    }
}

impl Config {
    // FIXME @alibektas : Server's health uses error sink but in other places it is not used atm.
    /// Changes made to client and global configurations will partially not be reflected even after `.apply_change()` was called.
    /// The return tuple's bool component signals whether the `GlobalState` should call its `update_configuration()` method.
    fn apply_change_with_sink(&self, change: ConfigChange) -> (Config, bool) {
        let mut config = self.clone();
        config.validation_errors = ConfigErrors::default();

        let mut should_update = false;

        if let Some(mut json) = change.client_config_change {
            tracing::info!("updating config from JSON: {:#}", json);

            if !(json.is_null() || json.as_object().is_some_and(|it| it.is_empty())) {
                let mut json_errors = vec![];
                config.client_config = (
                    ClientConfigInput::from_json(&mut json, &mut json_errors),
                    ConfigErrors(
                        json_errors
                            .into_iter()
                            .map(|(a, b)| ConfigErrorInner::Json { config_key: a, error: b })
                            .map(Arc::new)
                            .collect(),
                    ),
                );
            }
            should_update = true;
        }

        (config, should_update)
    }

    /// Given `change` this generates a new `Config`, thereby collecting errors of type `ConfigError`.
    /// If there are changes that have global/client level effect, the last component of the return type
    /// will be set to `true`, which should be used by the `GlobalState` to update itself.
    pub fn apply_change(&self, change: ConfigChange) -> (Config, ConfigErrors, bool) {
        let (config, should_update) = self.apply_change_with_sink(change);
        let e = ConfigErrors(
            config
                .client_config
                .1
                .0
                .iter()
                .chain(config.validation_errors.0.iter())
                .cloned()
                .collect(),
        );
        (config, e, should_update)
    }
}

#[derive(Default, Debug)]
pub struct ConfigChange {
    client_config_change: Option<serde_json::Value>,
}

impl ConfigChange {
    pub fn change_client_config(&mut self, change: serde_json::Value) {
        self.client_config_change = Some(change);
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HoverActionsConfig {
    pub implementations: bool,
    pub references: bool,
    pub run: bool,
    pub debug: bool,
    pub goto_type_def: bool,
}

impl HoverActionsConfig {
    pub const NO_ACTIONS: Self = Self {
        implementations: false,
        references: false,
        run: false,
        debug: false,
        goto_type_def: false,
    };

    pub fn any(&self) -> bool {
        self.implementations || self.references || self.runnable() || self.goto_type_def
    }

    pub fn none(&self) -> bool {
        !self.any()
    }

    pub fn runnable(&self) -> bool {
        self.run || self.debug
    }
}

#[derive(Debug, Clone)]
pub struct FilesConfig {
    pub watcher: FilesWatcher,
    pub exclude: Vec<AbsPathBuf>,
}

#[derive(Debug, Clone)]
pub enum FilesWatcher {
    Client,
    Server,
}

#[derive(Debug, Clone)]
pub struct NotificationsConfig {
    pub cargo_toml_not_found: bool,
}

#[derive(Debug, Clone)]
pub enum RustfmtConfig {
    Rustfmt { extra_args: Vec<String>, enable_range_formatting: bool },
    CustomCommand { command: String, args: Vec<String> },
}

/// Configuration for runnable items, such as `main` function or tests.
#[derive(Debug, Clone)]
pub struct RunnablesConfig {
    /// Custom command to be executed instead of `cargo` for runnables.
    pub override_cargo: Option<String>,
    /// Additional arguments for the `cargo`, e.g. `--release`.
    pub cargo_extra_args: Vec<String>,
    /// Additional arguments for the binary being run, if it is a test or benchmark.
    pub extra_test_binary_args: Vec<String>,
}

/// Configuration for workspace symbol search requests.
#[derive(Debug, Clone)]
pub struct WorkspaceSymbolConfig {
    /// In what scope should the symbol be searched in.
    pub search_scope: WorkspaceSymbolSearchScope,
    /// What kind of symbol is being searched for.
    pub search_kind: WorkspaceSymbolSearchKind,
    /// How many items are returned at most.
    pub search_limit: usize,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ClientCommandsConfig {
    pub run_single: bool,
    pub debug_single: bool,
    pub show_reference: bool,
    pub goto_location: bool,
    pub trigger_parameter_hints: bool,
    pub rename: bool,
}

#[derive(Debug)]
pub enum ConfigErrorInner {
    Json { config_key: String, error: serde_json::Error },
    Toml { config_key: String, error: toml::de::Error },
    ParseError { reason: String },
}

#[derive(Clone, Debug, Default)]
pub struct ConfigErrors(Vec<Arc<ConfigErrorInner>>);

impl ConfigErrors {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl fmt::Display for ConfigErrors {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let errors = self.0.iter().format_with("\n", |inner, f| match &**inner {
            ConfigErrorInner::Json { config_key: key, error: e } => {
                f(key)?;
                f(&": ")?;
                f(e)
            }
            ConfigErrorInner::Toml { config_key: key, error: e } => {
                f(key)?;
                f(&": ")?;
                f(e)
            }
            ConfigErrorInner::ParseError { reason } => f(reason),
        });
        write!(f, "invalid config value{}:\n{}", if self.0.len() == 1 { "" } else { "s" }, errors)
    }
}

impl std::error::Error for ConfigErrors {}

impl Config {
    pub fn new(
        root_path: AbsPathBuf,
        caps: lsp_types::ClientCapabilities,
        workspace_roots: Vec<AbsPathBuf>,
        client_info: Option<lsp_types::ClientInfo>,
    ) -> Self {
        static DEFAULT_CONFIG_DATA: OnceLock<&'static ClientDefaultConfigData> = OnceLock::new();

        Config {
            caps: ClientCapabilities::new(caps),
            discovered_projects: Vec::new(),
            root_path,
            workspace_roots,
            client_info: client_info.map(|it| ClientInfo {
                name: it.name,
                version: it.version.as_deref().map(Version::parse).and_then(Result::ok),
            }),
            client_config: (ClientConfigInput::default(), ConfigErrors(vec![])),
            default_config: DEFAULT_CONFIG_DATA.get_or_init(|| Box::leak(Box::default())),
            validation_errors: Default::default(),
        }
    }

    pub fn rediscover_workspaces(&mut self) {
        let discovered = ManifestPath::discover_all(&self.workspace_roots);
        tracing::info!("discovered projects: {:?}", discovered);
        if discovered.is_empty() {
            tracing::error!("failed to find any projects in {:?}", &self.workspace_roots);
        }
        self.discovered_projects = discovered;
    }

    pub fn remove_workspace(&mut self, path: &AbsPath) {
        if let Some(position) = self.workspace_roots.iter().position(|it| it == path) {
            self.workspace_roots.remove(position);
        }
    }

    pub fn add_workspaces(&mut self, paths: impl Iterator<Item = AbsPathBuf>) {
        self.workspace_roots.extend(paths);
    }

    fn schema_fields() -> Vec<SchemaField> {
        let mut fields = Vec::new();
        ClientConfigInput::schema_fields(&mut fields);
        fields.sort_by_key(|&(x, ..)| x);
        fields
    }

    pub fn json_schema() -> serde_json::Value {
        let mut s = schema(&Self::schema_fields());

        fn sort_objects_by_field(json: &mut serde_json::Value) {
            if let serde_json::Value::Object(object) = json {
                let old = std::mem::take(object);
                old.into_iter().sorted_by(|(k, _), (k2, _)| k.cmp(k2)).for_each(|(k, mut v)| {
                    sort_objects_by_field(&mut v);
                    object.insert(k, v);
                });
            }
        }
        sort_objects_by_field(&mut s);
        s
    }

    pub fn root_path(&self) -> &AbsPathBuf {
        &self.root_path
    }

    pub fn caps(&self) -> &ClientCapabilities {
        &self.caps
    }
}

impl Config {
    pub(crate) fn flycheck(&self) -> FlycheckConfig {
        FlycheckConfig::VipBuilderCommand {
            options: VipBuilderOptions::new(
                Some(BuildAction::Build),
                None,
                false,
                Option::default(),
            ),
        }
    }

    pub fn main_loop_num_threads(&self) -> usize {
        match self.numThreads() {
            Some(NumThreads::Concrete(0)) | None | Some(NumThreads::Physical) => {
                num_cpus::get_physical()
            }
            &Some(NumThreads::Concrete(n)) => n,
            Some(NumThreads::Logical) => num_cpus::get(),
        }
    }

    pub fn prefill_caches(&self) -> bool {
        self.cachePriming_enable().to_owned()
    }

    pub fn prime_caches_num_threads(&self) -> usize {
        match self.cachePriming_numThreads() {
            NumThreads::Concrete(0) | NumThreads::Physical => num_cpus::get_physical(),
            &NumThreads::Concrete(n) => n,
            NumThreads::Logical => num_cpus::get(),
        }
    }

    pub fn check_on_save(&self) -> bool {
        *self.checkOnSave()
    }

    // VSCode is our reference implementation, so we allow ourselves to work around issues by
    // special casing certain versions
    pub fn visual_studio_code_version(&self) -> Option<&Version> {
        self.client_info
            .as_ref()
            .filter(|it| it.name.starts_with("Visual Studio Code"))
            .and_then(|it| it.version.as_ref())
    }

    pub fn client_is_helix(&self) -> bool {
        self.client_info.as_ref().map(|it| it.name == "helix").unwrap_or_default()
    }

    pub fn client_is_neovim(&self) -> bool {
        self.client_info.as_ref().map(|it| it.name == "Neovim").unwrap_or_default()
    }

    pub fn workspace_ide_variables(&self) -> IdeVariables {
        IdeVariables::new(self.workspace_ideVariables().clone())
    }
}
// Deserialization definitions

macro_rules! create_bool_or_string_serde {
    ($ident:ident<$bool:literal, $string:literal>) => {
        mod $ident {
            pub(super) fn deserialize<'de, D>(d: D) -> Result<(), D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                struct V;
                impl<'de> serde::de::Visitor<'de> for V {
                    type Value = ();

                    fn expecting(
                        &self,
                        formatter: &mut std::fmt::Formatter<'_>,
                    ) -> std::fmt::Result {
                        formatter.write_str(concat!(
                            stringify!($bool),
                            " or \"",
                            stringify!($string),
                            "\""
                        ))
                    }

                    fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
                    where
                        E: serde::de::Error,
                    {
                        match v {
                            $bool => Ok(()),
                            _ => Err(serde::de::Error::invalid_value(
                                serde::de::Unexpected::Bool(v),
                                &self,
                            )),
                        }
                    }

                    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
                    where
                        E: serde::de::Error,
                    {
                        match v {
                            $string => Ok(()),
                            _ => Err(serde::de::Error::invalid_value(
                                serde::de::Unexpected::Str(v),
                                &self,
                            )),
                        }
                    }

                    fn visit_enum<A>(self, a: A) -> Result<Self::Value, A::Error>
                    where
                        A: serde::de::EnumAccess<'de>,
                    {
                        use serde::de::VariantAccess;
                        let (variant, va) = a.variant::<&'de str>()?;
                        va.unit_variant()?;
                        match variant {
                            $string => Ok(()),
                            _ => Err(serde::de::Error::invalid_value(
                                serde::de::Unexpected::Str(variant),
                                &self,
                            )),
                        }
                    }
                }
                d.deserialize_any(V)
            }

            pub(super) fn serialize<S>(serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                serializer.serialize_str($string)
            }
        }
    };
}
create_bool_or_string_serde!(true_or_always<true, "always">);
create_bool_or_string_serde!(false_or_never<false, "never">);

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
#[serde(rename_all = "snake_case")]
#[derive(Default)]
enum SnippetScopeDef {
    #[default]
    Expr,
    Item,
    Type,
}

#[derive(Serialize, Deserialize, Debug, Clone, Default)]
#[serde(default)]
pub(crate) struct SnippetDef {
    #[serde(with = "single_or_array")]
    #[serde(skip_serializing_if = "Vec::is_empty")]
    prefix: Vec<String>,

    #[serde(with = "single_or_array")]
    #[serde(skip_serializing_if = "Vec::is_empty")]
    postfix: Vec<String>,

    #[serde(with = "single_or_array")]
    #[serde(skip_serializing_if = "Vec::is_empty")]
    body: Vec<String>,

    #[serde(with = "single_or_array")]
    #[serde(skip_serializing_if = "Vec::is_empty")]
    requires: Vec<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    description: Option<String>,

    scope: SnippetScopeDef,
}

mod single_or_array {
    use serde::{Deserialize, Serialize};

    pub(super) fn deserialize<'de, D>(deserializer: D) -> Result<Vec<String>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct SingleOrVec;

        impl<'de> serde::de::Visitor<'de> for SingleOrVec {
            type Value = Vec<String>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                formatter.write_str("string or array of strings")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(vec![value.to_owned()])
            }

            fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                Deserialize::deserialize(serde::de::value::SeqAccessDeserializer::new(seq))
            }
        }

        deserializer.deserialize_any(SingleOrVec)
    }

    pub(super) fn serialize<S>(vec: &[String], serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match vec {
            // []  case is handled by skip_serializing_if
            [single] => serializer.serialize_str(single),
            slice => slice.serialize(serializer),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct CheckOnSaveTargets(#[serde(with = "single_or_array")] Vec<String>);

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
enum LifetimeElisionDef {
    SkipTrivial,
    #[serde(with = "true_or_always")]
    #[serde(untagged)]
    Always,
    #[serde(with = "false_or_never")]
    #[serde(untagged)]
    Never,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
enum ClosureReturnTypeHintsDef {
    WithBlock,
    #[serde(with = "true_or_always")]
    #[serde(untagged)]
    Always,
    #[serde(with = "false_or_never")]
    #[serde(untagged)]
    Never,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
enum ClosureStyle {
    ImplFn,
    RustAnalyzer,
    WithId,
    Hide,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
enum ReborrowHintsDef {
    Mutable,
    #[serde(with = "true_or_always")]
    #[serde(untagged)]
    Always,
    #[serde(with = "false_or_never")]
    #[serde(untagged)]
    Never,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
enum AdjustmentHintsDef {
    Reborrow,
    #[serde(with = "true_or_always")]
    #[serde(untagged)]
    Always,
    #[serde(with = "false_or_never")]
    #[serde(untagged)]
    Never,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
enum DiscriminantHintsDef {
    Fieldless,
    #[serde(with = "true_or_always")]
    #[serde(untagged)]
    Always,
    #[serde(with = "false_or_never")]
    #[serde(untagged)]
    Never,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
enum AdjustmentHintsModeDef {
    Prefix,
    Postfix,
    PreferPrefix,
    PreferPostfix,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
enum FilesWatcherDef {
    Client,
    Notify,
    Server,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
enum ImportPrefixDef {
    Plain,
    #[serde(rename = "self")]
    #[serde(alias = "by_self")]
    BySelf,
    #[serde(rename = "crate")]
    #[serde(alias = "by_crate")]
    ByCrate,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
enum WorkspaceSymbolSearchScopeDef {
    Workspace,
    WorkspaceAndDependencies,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
enum SignatureDetail {
    Full,
    Parameters,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
enum WorkspaceSymbolSearchKindDef {
    OnlyTypes,
    AllSymbols,
}

#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq)]
#[serde(rename_all = "snake_case")]
enum MemoryLayoutHoverRenderKindDef {
    Decimal,
    Hexadecimal,
    Both,
}

#[test]
fn untagged_option_hover_render_kind() {
    let hex = MemoryLayoutHoverRenderKindDef::Hexadecimal;

    let ser = serde_json::to_string(&Some(hex)).unwrap();
    assert_eq!(&ser, "\"hexadecimal\"");

    let opt: Option<_> = serde_json::from_str("\"hexadecimal\"").unwrap();
    assert_eq!(opt, Some(hex));
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "snake_case")]
#[serde(untagged)]
pub enum TargetDirectory {
    UseSubdirectory(bool),
    Directory(Utf8PathBuf),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum NumThreads {
    Physical,
    Logical,
    #[serde(untagged)]
    Concrete(usize),
}

macro_rules! _default_val {
    (@verbatim: $s:literal, $ty:ty) => {{
        let default_: $ty = serde_json::from_str(&$s).unwrap();
        default_
    }};
    ($default:expr, $ty:ty) => {{
        let default_: $ty = $default;
        default_
    }};
}
use _default_val as default_val;

macro_rules! _default_str {
    (@verbatim: $s:literal, $_ty:ty) => {
        $s.to_owned()
    };
    ($default:expr, $ty:ty) => {{
        let val = default_val!($default, $ty);
        serde_json::to_string_pretty(&val).unwrap()
    }};
}
use _default_str as default_str;

macro_rules! _impl_for_config_data {
    (local, $(
            $(#[doc=$doc:literal])*
            $vis:vis $field:ident : $ty:ty = $default:expr,
        )*
    ) => {
        impl Config {
            $(
                $($doc)*
                #[allow(non_snake_case)]
                $vis fn $field(&self, source_root: Option<SourceRootId>) -> &$ty {
                    if let Some(v) = self.client_config.0.local.$field.as_ref() {
                        return &v;
                    }

                    &self.default_config.local.$field
                }
            )*
        }
    };
    (global, $(
            $(#[doc=$doc:literal])*
            $vis:vis $field:ident : $ty:ty = $default:expr,
        )*
    ) => {
        impl Config {
            $(
                $($doc)*
                #[allow(non_snake_case)]
                $vis fn $field(&self) -> &$ty {

                    if let Some(v) = self.client_config.0.global.$field.as_ref() {
                        return &v;
                    }

                    &self.default_config.global.$field
                }
            )*
        }
    };
    (client, $(
            $(#[doc=$doc:literal])*
            $vis:vis $field:ident : $ty:ty = $default:expr,
       )*
    ) => {
        impl Config {
            $(
                $($doc)*
                #[allow(non_snake_case)]
                $vis fn $field(&self) -> &$ty {
                    if let Some(v) = self.client_config.0.$field.as_ref() {
                        return &v;
                    }

                    &self.default_config.$field
                }
            )*
        }
    };
}
use _impl_for_config_data as impl_for_config_data;

macro_rules! _config_data {
    // modname is for the tests
    ($(#[doc=$dox:literal])* $modname:ident: struct $name:ident <- $input:ident -> {
        $(
            $(#[doc=$doc:literal])*
            $vis:vis $field:ident $(| $alias:ident)*: $ty:ty = $(@$marker:ident: )? $default:expr,
        )*
    }) => {
        /// Default config values for this grouping.
        #[allow(non_snake_case)]
        #[derive(Debug, Clone )]
        struct $name { $($field: $ty,)* }

        impl_for_config_data!{
            $modname,
            $(
                $vis $field : $ty = $default,
            )*
        }

        /// All fields `Option<T>`, `None` representing fields not set in a particular JSON/TOML blob.
        #[allow(non_snake_case)]
        #[derive(Clone, Default)]
        struct $input { $(
            $field: Option<$ty>,
        )* }

        impl std::fmt::Debug for $input {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let mut s = f.debug_struct(stringify!($input));
                $(
                    if let Some(val) = self.$field.as_ref() {
                        s.field(stringify!($field), val);
                    }
                )*
                s.finish()
            }
        }

        impl Default for $name {
            fn default() -> Self {
                $name {$(
                    $field: default_val!($(@$marker:)? $default, $ty),
                )*}
            }
        }

        #[allow(unused, clippy::ptr_arg)]
        impl $input {
            const FIELDS: &'static [&'static str] = &[$(stringify!($field)),*];

            fn from_json(json: &mut serde_json::Value, error_sink: &mut Vec<(String, serde_json::Error)>) -> Self {
                Self {$(
                    $field: get_field_json(
                        json,
                        error_sink,
                        stringify!($field),
                        None$(.or(Some(stringify!($alias))))*,
                    ),
                )*}
            }

            fn schema_fields(sink: &mut Vec<SchemaField>) {
                sink.extend_from_slice(&[
                    $({
                        let field = stringify!($field);
                        let ty = stringify!($ty);
                        let default = default_str!($(@$marker:)? $default, $ty);

                        (field, ty, &[$($doc),*], default)
                    },)*
                ])
            }
        }

        mod $modname {
            #[test]
            fn fields_are_sorted() {
                super::$input::FIELDS.windows(2).for_each(|w| assert!(w[0] <= w[1], "{} <= {} does not hold", w[0], w[1]));
            }
        }
    };
}
use _config_data as config_data;

fn get_field_json<T: DeserializeOwned>(
    json: &mut serde_json::Value,
    error_sink: &mut Vec<(String, serde_json::Error)>,
    field: &'static str,
    alias: Option<&'static str>,
) -> Option<T> {
    // XXX: check alias first, to work around the VS Code where it pre-fills the
    // defaults instead of sending an empty object.
    alias
        .into_iter()
        .chain(iter::once(field))
        .filter_map(move |field| {
            let mut pointer = field.replace('_', "/");
            pointer.insert(0, '/');
            json.pointer_mut(&pointer)
                .map(|it| serde_json::from_value(it.take()).map_err(|e| (e, pointer)))
        })
        .find(Result::is_ok)
        .and_then(|res| match res {
            Ok(it) => Some(it),
            Err((e, pointer)) => {
                tracing::warn!("Failed to deserialize config field at {}: {:?}", pointer, e);
                error_sink.push((pointer, e));
                None
            }
        })
}

type SchemaField = (&'static str, &'static str, &'static [&'static str], String);

fn schema(fields: &[SchemaField]) -> serde_json::Value {
    let map = fields
        .iter()
        .map(|(field, ty, doc, default)| {
            let name = field.replace('_', ".");
            let category =
                name.find('.').map(|end| String::from(&name[..end])).unwrap_or("general".into());
            let name = format!("vip-analyzer.{name}");
            let props = field_props(field, ty, doc, default);
            serde_json::json!({
                "title": category,
                "properties": {
                    name: props
                }
            })
        })
        .collect::<Vec<_>>();
    map.into()
}

fn field_props(field: &str, ty: &str, doc: &[&str], default: &str) -> serde_json::Value {
    let doc = doc_comment_to_string(doc);
    let doc = doc.trim_end_matches('\n');
    assert!(
        doc.ends_with('.') && doc.starts_with(char::is_uppercase),
        "bad docs for {field}: {doc:?}"
    );
    let default = default.parse::<serde_json::Value>().unwrap();

    let mut map = serde_json::Map::default();
    macro_rules! set {
        ($($key:literal: $value:tt),*$(,)?) => {{$(
            map.insert($key.into(), serde_json::json!($value));
        )*}};
    }
    set!("markdownDescription": doc);
    set!("default": default);

    match ty {
        "bool" => set!("type": "boolean"),
        "usize" => set!("type": "integer", "minimum": 0),
        "String" => set!("type": "string"),
        "Vec<String>" => set! {
            "type": "array",
            "items": { "type": "string" },
        },
        "Vec<Utf8PathBuf>" => set! {
            "type": "array",
            "items": { "type": "string" },
        },
        "FxHashSet<String>" => set! {
            "type": "array",
            "items": { "type": "string" },
            "uniqueItems": true,
        },
        "FxHashMap<Box<str>, Box<[Box<str>]>>" => set! {
            "type": "object",
        },
        "FxHashMap<String, SnippetDef>" => set! {
            "type": "object",
        },
        "FxHashMap<String, String>" => set! {
            "type": "object",
        },
        "FxHashMap<Box<str>, usize>" => set! {
            "type": "object",
        },
        "FxHashMap<String, Option<String>>" => set! {
            "type": "object",
        },
        "FxHashMap<ManifestPath, ManifestVars>" => set! {
            "type": "object",
        },
        "Option<usize>" => set! {
            "type": ["null", "integer"],
            "minimum": 0,
        },
        "Option<u16>" => set! {
            "type": ["null", "integer"],
            "minimum": 0,
            "maximum": 65535,
        },
        "Option<String>" => set! {
            "type": ["null", "string"],
        },
        "Option<Utf8PathBuf>" => set! {
            "type": ["null", "string"],
        },
        "Option<bool>" => set! {
            "type": ["null", "boolean"],
        },
        "Option<Vec<String>>" => set! {
            "type": ["null", "array"],
            "items": { "type": "string" },
        },
        "NumThreads" => set! {
            "anyOf": [
                {
                    "type": "number",
                    "minimum": 0,
                    "maximum": 255
                },
                {
                    "type": "string",
                    "enum": ["physical", "logical", ],
                    "enumDescriptions": [
                        "Use the number of physical cores",
                        "Use the number of logical cores",
                    ],
                },
            ],
        },
        "Option<NumThreads>" => set! {
            "anyOf": [
                {
                    "type": "null"
                },
                {
                    "type": "number",
                    "minimum": 0,
                    "maximum": 255
                },
                {
                    "type": "string",
                    "enum": ["physical", "logical", ],
                    "enumDescriptions": [
                        "Use the number of physical cores",
                        "Use the number of logical cores",
                    ],
                },
            ],
        },

        _ => panic!("missing entry for {ty}: {default}"),
    }

    map.into()
}

fn doc_comment_to_string(doc: &[&str]) -> String {
    doc.iter()
        .map(|it| it.strip_prefix(' ').unwrap_or(it))
        .fold(String::new(), |mut acc, it| format_to_acc!(acc, "{it}\n"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use test_utils::{ensure_file_contents, project_root};

    #[test]
    fn generate_package_json_config() {
        let s = Config::json_schema();

        let schema = format!("{s:#}");
        let mut schema = schema
            .trim_start_matches('[')
            .trim_end_matches(']')
            .replace("  ", "    ")
            .replace('\n', "\n        ")
            .trim_start_matches('\n')
            .trim_end()
            .to_owned();
        schema.push_str(",\n");

        // Transform the asciidoc form link to markdown style.
        //
        // https://link[text] => [text](https://link)
        let url_matches = schema.match_indices("https://");
        let mut url_offsets = url_matches.map(|(idx, _)| idx).collect::<Vec<usize>>();
        url_offsets.reverse();
        for idx in url_offsets {
            let link = &schema[idx..];
            // matching on whitespace to ignore normal links
            if let Some(link_end) = link.find([' ', '[']) {
                if link.chars().nth(link_end) == Some('[') {
                    if let Some(link_text_end) = link.find(']') {
                        let link_text = link[link_end..(link_text_end + 1)].to_string();

                        schema.replace_range((idx + link_end)..(idx + link_text_end + 1), "");
                        schema.insert(idx, '(');
                        schema.insert(idx + link_end + 1, ')');
                        schema.insert_str(idx, &link_text);
                    }
                }
            }
        }

        let package_json_path = project_root().join("editors/code/package.json");
        let mut package_json = fs::read_to_string(&package_json_path).unwrap();

        let start_marker =
            "            {\n                \"title\": \"$generated-start\"\n            },\n";
        let end_marker =
            "            {\n                \"title\": \"$generated-end\"\n            }\n";

        let start = package_json.find(start_marker).unwrap() + start_marker.len();
        let end = package_json.find(end_marker).unwrap();

        // let p = remove_ws(&package_json[start..end]);
        // let s = remove_ws(&schema);
        // if !p.contains(&s) {
        package_json.replace_range(start..end, &schema);
        ensure_file_contents(package_json_path.as_std_path(), &package_json);
        // }
    }

    // fn remove_ws(text: &str) -> String {
    //     text.replace(char::is_whitespace, "")
    // }
}
