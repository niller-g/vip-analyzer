//! Applies changes to the IDE state transactionally.

use crate::RootDatabase;
use salsa::{Database as _, Durability};

impl RootDatabase {
    pub fn request_cancellation(&mut self) {
        let _p = tracing::info_span!("RootDatabase::request_cancellation").entered();
        self.synthetic_write(Durability::LOW);
    }

    pub fn apply_change(&mut self, change: base_db::FileChange) {
        let _p = tracing::info_span!("RootDatabase::apply_change").entered();
        self.request_cancellation();
        tracing::trace!("apply_change {:?}", change);
        change.apply(self);
    }
}
