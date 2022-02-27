use std::{io, num::ParseIntError};

use kdl::KdlNode;

use crate::{resolver::DependencyErrors, KdlType, KdlTypeTuple};

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    IoError(#[from] io::Error),

    #[error("glob")]
    GlobError(#[from] glob::GlobError),

    #[error("glob")]
    PatternError(#[from] glob::PatternError),

    #[error("kdl parser")]
    KdlError(#[from] kdl::KdlError),

    #[error("bincode")]
    BinCode(#[from] bincode::Error),

    #[error("required property {property} ({ty}) was missing in `{node}`")]
    RequiredPropertyMissing {
        property: &'static str,
        ty: KdlType,
        node: KdlNode,
    },

    #[error("wrong type for {property} in `{node}`")]
    PropertyTypeMismatch {
        property: &'static str,
        source: kdl::TryFromKdlNodeValueError,
        node: KdlNode,
    },

    #[error("unexpected property {property:?} in `{node}`")]
    UnknownProperty { property: String, node: KdlNode },

    #[error("expected values {expected}, got {actual} in `{node}`")]
    ValuesMismatch {
        expected: KdlTypeTuple,
        actual: KdlTypeTuple,
        node: KdlNode,
    },

    #[error("wrong type for value {position} in `{node}`")]
    ValueMismatch {
        position: usize,
        source: kdl::TryFromKdlNodeValueError,
        node: KdlNode,
    },

    #[error("value {position} is missing, expected {expected} in `{node}`")]
    ValueMissing {
        position: usize,
        expected: KdlType,
        node: KdlNode,
    },

    #[error("unknown node `{node}`")]
    UnknownNodeTag { node: KdlNode },

    #[error("{child} is not allowed inside {parent}")]
    UnexpectedNode {
        parent: &'static str,
        child: &'static str,
    },

    #[error("invalid dimensionality `{string}`")]
    InvalidDimensionality {
        string: String,
        source: ParseIntError,
    },

    #[error(transparent)]
    DependencyErrors(#[from] DependencyErrors<String>),
}

impl Error {
    pub(crate) fn values_mismatch(node: &KdlNode, expected: Vec<KdlType>) -> Error {
        Error::ValuesMismatch {
            expected: KdlTypeTuple(expected),
            actual: KdlTypeTuple(node.values.iter().map(KdlType::from).collect::<Vec<_>>()),
            node: node.clone(),
        }
    }
}
