//! This crate models the toolchain used in in a visual prolog project

mod diagnostic_codes;

use std::{ffi::OsStr, fmt, path::Path, process::Command, str};

use project_model::VariablePath;

pub fn command(
    cmd: impl AsRef<OsStr>,
    working_directory: impl AsRef<Path>,
    // extra_env: &std::collections::HashMap<String, Option<String>, H>,
) -> Command {
    // we are `toolchain::command``
    #[allow(clippy::disallowed_methods)]
    let mut cmd = Command::new(cmd);
    cmd.current_dir(working_directory);
    // for env in extra_env {
    //     match env {
    //         (key, Some(val)) => cmd.env(key, val),
    //         (key, None) => cmd.env_remove(key),
    //     };
    // }
    cmd
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct VipBuilderOptions {
    pub action: Option<BuildAction>,
    platform: Option<VipPlatform>,
    silent: bool,

    // Additional compiler options
    debug: Option<DebugLevel>,
}
impl VipBuilderOptions {
    pub fn new(
        action: Option<BuildAction>,
        platform: Option<VipPlatform>,
        silent: bool,
        debug: Option<DebugLevel>,
    ) -> Self {
        VipBuilderOptions { action, platform, silent, debug }
    }

    pub fn apply_on_command(&self, cmd: &mut Command, manifest_path: &Path) {
        if let Some(action) = &self.action {
            match action {
                BuildAction::Build => cmd.arg("/build"),
                BuildAction::Rebuild => cmd.arg("/rebuild"),
            };
        };
        if let Some(platform) = &self.platform {
            cmd.arg("/platform").arg(format!("/{platform}"));
        };
        if self.silent {
            cmd.arg("/s");
        }
        cmd.arg(manifest_path);
        if let Some(debug) = &self.debug {
            cmd.args(["/options", &format!("debug:{debug}")]);
        }
    }
}
impl fmt::Display for VipBuilderOptions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut options = Vec::new();
        if let Some(action) = &self.action {
            options.push("/".to_owned());
            options.push(action.to_string());
        }
        if let Some(platform) = &self.platform {
            options.push(format!("/platform {platform}"));
        }
        if self.silent {
            options.push("/s".into());
        }

        if let Some(debug) = &self.debug {
            options.push(format!("/options debug:{debug}"));
        }

        write!(f, "{}", options.join(" "))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub enum BuildAction {
    #[default]
    Build,
    Rebuild,
}
impl std::fmt::Display for BuildAction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuildAction::Build => write!(f, "build"),
            BuildAction::Rebuild => write!(f, "rebuild"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VipPlatform {
    All,
    Win32,
    X64,
}
impl std::fmt::Display for VipPlatform {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VipPlatform::All => write!(f, "all"),
            VipPlatform::Win32 => write!(f, "Win32"),
            VipPlatform::X64 => write!(f, "x64"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DebugLevel {
    None,
    LineNo,
    Full,
}
impl std::fmt::Display for DebugLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DebugLevel::None => write!(f, "none"),
            DebugLevel::LineNo => write!(f, "lineno"),
            DebugLevel::Full => write!(f, "full"),
        }
    }
}

pub fn as_file_compiled_msg(str: &str) -> Option<&str> {
    const PREFIX: &str = "file '";
    const SUFFIX: &str = "' compiled";

    str.strip_prefix(PREFIX).and_then(|s| s.strip_suffix(SUFFIX))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VipBuilderDiagnostic {
    /// The error message of this diagnostic.
    pub message: String,
    /// The associated error code for this diagnostic
    pub code: Option<VipDiagnosticCode>,
    /// "error", "warning", "fatal", "information", etc.
    pub severity: Severity,
    /// Source code span this diagnostic is associated with.
    pub span: Option<DiagnosticSpan>,
}
impl VipBuilderDiagnostic {
    pub fn new<Msg>(
        message: Msg,
        code: Option<VipDiagnosticCode>,
        severity: Severity,
        span: Option<DiagnosticSpan>,
    ) -> Self
    where
        Msg: Into<String>,
    {
        VipBuilderDiagnostic { message: message.into(), code, severity, span }
    }

    pub fn location(&self) -> Option<&Location> {
        match &self.span {
            Some(DiagnosticSpan::Location(location)) => Some(location),
            _ => None,
        }
    }
}
impl str::FromStr for VipBuilderDiagnostic {
    type Err = &'static str;

    fn from_str(str: &str) -> Result<Self, Self::Err> {
        let span_end = str.find(" : ");
        let span_end = match span_end {
            Some(span_end) => span_end,
            None => {
                return try_as_link_message(str)
                    .or_else(|_| try_as_sparse_message(str))
                    .map_err(|_| "expected `)`");
            }
        };

        let (span, rest) = str.split_at(span_end);
        let rest = rest.trim_start_matches(" : ");
        let severity_code_msg: Vec<_> = rest.splitn(2, ": ").collect();
        if severity_code_msg.len() != 2 {
            return Err("expected `: `");
        }
        let severity_code = severity_code_msg[0];
        let msg = severity_code_msg[1];
        let (severity, code) = severity_code.rsplit_once(" ").ok_or("expected ` `")?;
        let severity: Severity = severity.parse()?;
        let code: VipDiagnosticCode = code.parse()?;

        Ok(VipBuilderDiagnostic::new(
            msg,
            Some(code),
            severity,
            Some(DiagnosticSpan::as_span(span.trim_start())),
        ))
    }
}

/// Parses messages on the regex format `(error|fatal error|warning) \d+: .+`
fn try_as_link_message(msg: &str) -> Result<VipBuilderDiagnostic, &'static str> {
    fn parse_message(msg: &str, severity: Severity) -> Result<VipBuilderDiagnostic, &'static str> {
        let severity_str = severity.to_string();
        let severity_len = severity_str.len();

        if msg.get(..severity_len).map(str::to_lowercase) == Some(severity_str)
            && msg.get(severity_len..severity_len + " ".len()) == Some(" ")
        {
            let rest = &msg[severity_len + " ".len()..];
            let code_and_msg: Vec<_> = rest.splitn(2, ": ").collect();
            if code_and_msg.len() != 2 {
                return Err("expected `: `");
            }
            let code: VipDiagnosticCode = code_and_msg[0].parse()?;
            return Ok(VipBuilderDiagnostic::new(code_and_msg[1], Some(code), severity, None));
        }
        Err("not a link message")
    }

    parse_message(msg, Severity::Error)
        .or_else(|_| parse_message(msg, Severity::FatalError))
        .or_else(|_| parse_message(msg, Severity::Warning))
}

/// Parses messages on the regex format `"?.+"?\(\d+,\d+\)\s.+`
fn try_as_sparse_message(msg: &str) -> Result<VipBuilderDiagnostic, &'static str> {
    let span_end = msg.find(')').ok_or("expected `)`")?;
    let (span, msg) = msg.split_at(span_end + 1);
    let span: Location = span.parse()?;
    Ok(VipBuilderDiagnostic::new(
        msg.trim_start(),
        None,
        Severity::Error,
        Some(DiagnosticSpan::Location(span)),
    ))
}

impl fmt::Display for VipBuilderDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.span {
            Some(span) => match &self.code {
                Some(code) => write!(f, "{span} : {} {code}: {}", self.severity, self.message),
                None => write!(f, "{span} {}", self.message),
            },
            None => match &self.code {
                Some(code) => write!(f, "{} {code}: {}", self.severity.capitalized(), self.message),
                None => write!(f, "{} {}", self.severity, self.message),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VipDiagnosticCode {
    /// Normal diagnostic code like `c001` or `2503`
    Code(usize),
    /// Link diagnostic code like `LNK(2)`
    LnkCode(usize),
}
impl str::FromStr for VipDiagnosticCode {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() == 4 {
            let code =
                s.strip_prefix('c').unwrap_or(s).parse().map_err(|_| "failed to parse code")?;

            return Ok(VipDiagnosticCode::Code(code));
        } else if s.starts_with("LNK(") && s.ends_with(')') {
            let code = s
                .strip_prefix("LNK(")
                .unwrap()
                .strip_suffix(')')
                .unwrap()
                .parse()
                .map_err(|_| "failed to parse code")?;
            return Ok(VipDiagnosticCode::LnkCode(code));
        }
        Err("expected 4 characters in diagnostic code")
    }
}
impl From<usize> for VipDiagnosticCode {
    fn from(code: usize) -> Self {
        VipDiagnosticCode::Code(code)
    }
}
impl fmt::Display for VipDiagnosticCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VipDiagnosticCode::Code(code) => {
                if *code < 1000 {
                    write!(f, "c{code:03}")
                } else {
                    write!(f, "{code:04}")
                }
            }
            VipDiagnosticCode::LnkCode(code) => write!(f, "LNK({code})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Severity {
    SeeAlso,
    Info,
    Warning,
    Error,
    FatalError,
}
impl Severity {
    const fn capitalized(&self) -> &str {
        match self {
            Severity::SeeAlso => "See also",
            Severity::Info => "Information",
            Severity::Warning => "Warning",
            Severity::Error => "Error",
            Severity::FatalError => "Fatal error",
        }
    }
}
impl str::FromStr for Severity {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "see also" => Ok(Severity::SeeAlso),
            "information" => Ok(Severity::Info),
            "warning" => Ok(Severity::Warning),
            "error" => Ok(Severity::Error),
            "fatal error" | "fatal" => Ok(Severity::FatalError),
            _ => Err("unknown diagnostic severity"),
        }
    }
}
impl fmt::Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Severity::SeeAlso => write!(f, "see also"),
            Severity::Info => write!(f, "information"),
            Severity::Warning => write!(f, "warning"),
            Severity::Error => write!(f, "error"),
            Severity::FatalError => write!(f, "fatal error"),
        }
    }
}

/// A section of the source code associated with a Diagnostic
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiagnosticSpan {
    /// A location in the source code
    Location(Location),
    /// Some arbitrary name for this span. Only used when a location cannot be determined.
    Name(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    /// The file name or the macro name this diagnostic comes from.
    pub file_name: VariablePath,
    /// The line in the file. 1-based index.
    pub line_start: usize,
    /// The column in the file. 1-based index.
    pub column_start: usize,
}
impl str::FromStr for Location {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut begin_parens = s.char_indices().filter(|&(_, c)| c == '(').map(|(i, _)| i);
        let split_idx = if s.contains("$(") { begin_parens.nth(1) } else { begin_parens.next() }
            .ok_or("expected `(`")?;

        let (file_name, tail) = s.split_at(split_idx);
        let tail = tail.strip_prefix('(').unwrap();

        let file_name = file_name.trim_matches('"').into();

        // split at ,
        let parts: Vec<_> = tail.splitn(2, ',').collect();
        if parts.len() != 2 {
            return Err("expected `,`");
        }
        let line_start = parts[0].trim().parse().map_err(|_| "failed to parse line number")?;

        // split at )
        let parts: Vec<_> = parts[1].splitn(2, ')').collect();
        if parts.len() != 2 {
            return Err("expected `)`");
        }
        let column_start = parts[0].trim().parse().map_err(|_| "failed to parse column number")?;

        Ok(Location { file_name, line_start, column_start })
    }
}

impl DiagnosticSpan {
    pub fn new(file_name: VariablePath, line_start: usize, column_start: usize) -> Self {
        DiagnosticSpan::Location(Location { file_name, line_start, column_start })
    }
    pub fn name<T: Into<String>>(name: T) -> Self {
        DiagnosticSpan::Name(name.into())
    }

    fn as_span(s: &str) -> Self {
        if let Ok(location) = s.parse() {
            DiagnosticSpan::Location(location)
        } else {
            DiagnosticSpan::Name(s.to_owned())
        }
    }
}
impl fmt::Display for DiagnosticSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagnosticSpan::Location(Location { file_name, line_start, column_start }) => {
                write!(f, "{file_name}({line_start},{column_start})")
            }
            DiagnosticSpan::Name(name) => write!(f, "{name}"),
        }
    }
}

#[test]
fn compiler_message1() {
    let msg = "main.pro(1,1) : error c001: This is an error message";
    let diagnostic: VipBuilderDiagnostic = msg.parse().unwrap();
    assert_eq!(diagnostic.to_string(), msg);
    assert_eq!(
        diagnostic,
        VipBuilderDiagnostic::new(
            "This is an error message",
            Some(VipDiagnosticCode::from(1)),
            Severity::Error,
            Some(DiagnosticSpan::new("main.pro".into(), 1, 1)),
        )
    );
}

#[test]
fn compiler_message2() {
    let msg = r"$(ProDir)pfc\core.cl(15,1) : error c150: Syntax error";
    let diagnostic: VipBuilderDiagnostic = msg.parse().unwrap();
    assert_eq!(diagnostic.to_string(), msg);
    assert_eq!(
        diagnostic,
        VipBuilderDiagnostic::new(
            "Syntax error",
            Some(VipDiagnosticCode::from(150)),
            Severity::Error,
            Some(DiagnosticSpan::Location(Location {
                file_name: r"$(ProDir)pfc\core.cl".into(),
                line_start: 15,
                column_start: 1
            }))
        )
    );
}

#[test]
fn link_message() {
    let msg = r#"Error 2525: '..\.vip\dummyProject\obj\taskWindow.B668E84D.obj' - undefined name '?_display@aboutDialog@@SAXQAVwindow@@@PAPAV1@@@Z'"#;
    let diagnostic: VipBuilderDiagnostic = msg.parse().unwrap();
    assert_eq!(diagnostic.to_string(), msg);
    assert_eq!(
        diagnostic,
        VipBuilderDiagnostic::new(
            r#"'..\.vip\dummyProject\obj\taskWindow.B668E84D.obj' - undefined name '?_display@aboutDialog@@SAXQAVwindow@@@PAPAV1@@@Z'"#,
            Some(VipDiagnosticCode::from(2525)),
            Severity::Error,
            None
        )
    );
}

#[test]
fn link_message_2() {
    let msg = r#"Error 2528: 'Symbol redefinition in file '..\..\Obj\FlightOps64\ApplicationTestPresenter.0360A096.obj': 'lib\test\applicationTestPresenter?_initObject@@@QAGXQAV1@H@Z' (see '..\..\Obj\FlightOps64\ApplicationTestPresenter.27B83C6F.obj')"#;
    let diagnostic: VipBuilderDiagnostic = msg.parse().unwrap();
    assert_eq!(diagnostic.to_string(), msg);
    assert_eq!(
        diagnostic,
        VipBuilderDiagnostic::new(
            r#"'Symbol redefinition in file '..\..\Obj\FlightOps64\ApplicationTestPresenter.0360A096.obj': 'lib\test\applicationTestPresenter?_initObject@@@QAGXQAV1@H@Z' (see '..\..\Obj\FlightOps64\ApplicationTestPresenter.27B83C6F.obj')"#,
            Some(VipDiagnosticCode::from(2528)),
            Severity::Error,
            None
        )
    );
}

#[test]
fn link_fatal_message() {
    let msg = r#"Fatal error 2503: ..\.vip\dummyProject\obj\taskWindow.B668E84D.obj"#;
    let diagnostic: VipBuilderDiagnostic = msg.parse().unwrap();
    assert_eq!(diagnostic.to_string(), msg);
    assert_eq!(
        diagnostic,
        VipBuilderDiagnostic::new(
            r#"..\.vip\dummyProject\obj\taskWindow.B668E84D.obj"#,
            Some(VipDiagnosticCode::from(2503)),
            Severity::FatalError,
            None
        )
    );
}

#[test]
fn ms_link_message1() {
    let msg = r#"a : warning LNK(42): bla bla"#;
    let diagnostic: VipBuilderDiagnostic = msg.parse().unwrap();
    assert_eq!(diagnostic.to_string(), msg);
    assert_eq!(
        diagnostic,
        VipBuilderDiagnostic::new(
            "bla bla",
            Some(VipDiagnosticCode::LnkCode(42)),
            Severity::Warning,
            Some(DiagnosticSpan::Name("a".into()))
        )
    );
}

#[test]
fn ms_link_message2() {
    let msg = r#"a(b) : fatal error LNK(42): bla bla"#;
    let diagnostic: VipBuilderDiagnostic = msg.parse().unwrap();
    assert_eq!(diagnostic.to_string(), msg);
    assert_eq!(
        diagnostic,
        VipBuilderDiagnostic::new(
            "bla bla",
            Some(VipDiagnosticCode::LnkCode(42)),
            Severity::FatalError,
            Some(DiagnosticSpan::Name("a(b)".into()))
        )
    );
}

#[test]
fn ms_link_message3() {
    let msg = r#"LINK : error LNK(42): bla bla"#;
    let diagnostic: VipBuilderDiagnostic = msg.parse().unwrap();
    assert_eq!(diagnostic.to_string(), msg);
    assert_eq!(
        diagnostic,
        VipBuilderDiagnostic::new(
            "bla bla",
            Some(VipDiagnosticCode::LnkCode(42)),
            Severity::Error,
            Some(DiagnosticSpan::Name("LINK".into()))
        )
    );
}

#[test]
fn sparse_message1() {
    let msg = r#"main.pro(1,1) This is an error message"#;
    let diagnostic: VipBuilderDiagnostic = msg.parse().unwrap();
    assert_eq!(diagnostic.to_string(), msg);
    assert_eq!(
        diagnostic,
        VipBuilderDiagnostic::new(
            "This is an error message",
            None,
            Severity::Error,
            Some(DiagnosticSpan::new("main.pro".into(), 1, 1))
        )
    );
}

#[test]
fn sparse_message2() {
    let msg = r#""C:\proj\src\main.pro"(42,20) This is an error message"#;
    let diagnostic: VipBuilderDiagnostic = msg.parse().unwrap();
    assert_eq!(diagnostic.to_string(), r#"C:\proj\src\main.pro(42,20) This is an error message"#);
    assert_eq!(
        diagnostic,
        VipBuilderDiagnostic::new(
            "This is an error message",
            None,
            Severity::Error,
            Some(DiagnosticSpan::new(r#"C:\proj\src\main.pro"#.into(), 42, 20))
        )
    );
}

#[test]
fn as_file_compiled_msg1() {
    let msg = "file 'main.pack' compiled";
    assert_eq!(as_file_compiled_msg(msg), Some("main.pack"));
}

#[test]
fn as_file_compiled_msg2() {
    let msg = r"file 'C:\git\vip-analyzer\visual-prolog\ProDir\vip\vipSyntax\vipSyntaxSem\vipSyntaxSem.pack' compiled";
    assert_eq!(
        as_file_compiled_msg(msg),
        Some(
            r"C:\git\vip-analyzer\visual-prolog\ProDir\vip\vipSyntax\vipSyntaxSem\vipSyntaxSem.pack"
        )
    );
}
