use std::{borrow::Borrow, fmt, sync::Arc};

use serde_derive::{Deserialize, Serialize};

/// A newtype for a string dimension ID, so that we can implement traits for it.
#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[serde(transparent)]
pub struct BaseUnit {
    pub id: Arc<String>,
}

impl Borrow<str> for BaseUnit {
    fn borrow(&self) -> &str {
        &**self.id
    }
}

impl fmt::Display for BaseUnit {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.id.fmt(fmt)
    }
}

impl BaseUnit {
    pub fn new(dim: &str) -> BaseUnit {
        BaseUnit {
            id: Arc::new(dim.to_owned()),
        }
    }

    pub fn as_str(&self) -> &str {
        &self.id[..]
    }

    pub fn to_string(&self) -> String {
        self.as_str().to_owned()
    }
}
