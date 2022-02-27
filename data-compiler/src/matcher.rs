use core::fmt;

use kdl::{KdlNode, KdlValue, TryFromKdlNodeValueError};

use crate::Error;

#[derive(Copy, Clone, Debug, PartialEq, Eq, displaydoc::Display)]
pub enum KdlType {
    /// int
    Int,
    /// float
    Float,
    /// string
    String,
    /// boolean
    Boolean,
    /// null
    Null,
    /// {0}?
    Optional(&'static KdlType),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KdlTypeTuple(pub Vec<KdlType>);

impl fmt::Display for KdlTypeTuple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl<'a> From<&'a KdlValue> for KdlType {
    fn from(node: &'a KdlValue) -> Self {
        match node {
            KdlValue::Int(_) => KdlType::Int,
            KdlValue::Float(_) => KdlType::Float,
            KdlValue::String(_) => KdlType::String,
            KdlValue::Boolean(_) => KdlType::Boolean,
            KdlValue::Null => KdlType::Null,
        }
    }
}

pub trait KdlFromType {
    const TYPE: KdlType;
}

impl KdlFromType for String {
    const TYPE: KdlType = KdlType::String;
}

impl<T> KdlFromType for Option<T>
where
    T: KdlFromType,
{
    const TYPE: KdlType = KdlType::Optional(&T::TYPE);
}

pub trait KdlPrimitive {}
impl KdlPrimitive for String {}

pub trait FromKdl: Sized {
    fn from_props(node: &KdlNode, name: &'static str) -> Result<Self, Error>;
    fn from_values(node: &KdlNode, position: usize) -> Result<Self, Error>;
}

impl<T> FromKdl for Option<T>
where
    T: FromKdl,
{
    fn from_props(node: &KdlNode, name: &'static str) -> Result<Self, Error> {
        if let Some(_value) = node.properties.get(name) {
            Ok(Some(T::from_props(node, name)?))
        } else {
            Ok(None)
        }
    }

    fn from_values(node: &KdlNode, position: usize) -> Result<Self, Error> {
        if let Some(_value) = node.values.get(position) {
            Ok(Some(T::from_values(node, position)?))
        } else {
            Ok(None)
        }
    }
}

impl<T> FromKdl for T
where
    T: TryFrom<KdlValue, Error = TryFromKdlNodeValueError> + KdlFromType + KdlPrimitive,
{
    fn from_props(node: &KdlNode, name: &'static str) -> Result<Self, Error> {
        if let Some(value) = node.properties.get(name) {
            T::try_from(value.clone()).map_err(|source| Error::PropertyTypeMismatch {
                source,
                property: name,
                node: node.clone(),
            })
        } else {
            Err(Error::RequiredPropertyMissing {
                property: name,
                ty: T::TYPE,
                node: node.clone(),
            })
        }
    }

    fn from_values(node: &KdlNode, position: usize) -> Result<Self, Error> {
        if let Some(value) = node.values.get(position) {
            T::try_from(value.clone()).map_err(|source| Error::ValueMismatch {
                source,
                position,
                node: node.clone(),
            })
        } else {
            Err(Error::ValueMissing {
                position,
                node: node.clone(),
                expected: T::TYPE,
            })
        }
    }
}

pub trait TryFromKdlTuple: Sized {
    fn try_from_properties(node: &KdlNode, names: &[&'static str]) -> Result<Self, Error>;
    fn try_from_values(node: &KdlNode) -> Result<Self, Error>;
}

impl TryFromKdlTuple for () {
    fn try_from_properties(node: &KdlNode, names: &[&'static str]) -> Result<Self, Error> {
        assert_eq!(names.len(), 0);
        if node.properties.is_empty() {
            Ok(())
        } else {
            Err(Error::UnknownProperty {
                property: node.properties.values().nth(0).unwrap().to_string(),
                node: node.clone(),
            })
        }
    }

    fn try_from_values(node: &KdlNode) -> Result<Self, Error> {
        if node.values.is_empty() {
            Ok(())
        } else {
            Err(Error::ValuesMismatch {
                expected: KdlTypeTuple(vec![]),
                actual: KdlTypeTuple(node.values.iter().map(KdlType::from).collect::<Vec<_>>()),
                node: node.clone(),
            })
        }
    }
}

fn check_unknown_keys(node: &KdlNode, keys: &[&'static str]) -> Result<(), Error> {
    for key in node.properties.keys() {
        if !keys.contains(&&key[..]) {
            return Err(Error::UnknownProperty {
                property: key.to_string(),
                node: node.clone(),
            });
        }
    }
    Ok(())
}

macro_rules! impl_try_from_kdl_tuple {
    ($( $name:ident : $ty:ident ),+) => {
        impl<$( $ty ),+> TryFromKdlTuple for ($($ty),+,)
        where
            $(
                $ty: TryFrom<KdlValue, Error = TryFromKdlNodeValueError> + KdlFromType + FromKdl
            ),+
        {
            fn try_from_properties(node: &KdlNode, names: &[&'static str]) -> Result<Self, Error> {
                let mut i = 0;
                $(
                    let $name = $ty::from_props(node, names[i])?;
                    i += 1;
                )+
                assert_eq!(names.len(), i);
                check_unknown_keys(node, names)?;
                Ok((
                    $( $name ),+ ,
                ))
            }

            fn try_from_values(node: &KdlNode) -> Result<Self, Error> {
                let mut i = 0;
                $(
                    let $name = $ty::from_values(node, i);
                    i += 1;
                )+
                // In the case of Option<> values, the actual number provided may be less.
                if i >= node.values.len() {
                    if let ($(Ok($name)),+,) = ($($name),+,) {
                        return Ok(($($name),+,));
                    }
                }
                Err(Error::values_mismatch(node, vec![
                    $(
                        $ty::TYPE
                    ),+
                ]))
            }
        }
    };
}

impl_try_from_kdl_tuple!(a: A);
impl_try_from_kdl_tuple!(a: A, b: B);
impl_try_from_kdl_tuple!(a: A, b: B, c: C);
impl_try_from_kdl_tuple!(a: A, b: B, c: C, d: D);
impl_try_from_kdl_tuple!(a: A, b: B, c: C, d: D, e: E);
impl_try_from_kdl_tuple!(a: A, b: B, c: C, d: D, e: E, f: F);

pub fn expect_props<T>(node: &KdlNode, props: &[&'static str]) -> Result<T, Error>
where
    T: TryFromKdlTuple,
{
    T::try_from_properties(node, props)
}

pub fn expect_values<T>(node: &KdlNode) -> Result<T, Error>
where
    T: TryFromKdlTuple,
{
    T::try_from_values(node)
}

pub trait NodeShape {
    const NAME: &'static str;
    const PROP_NAMES: &'static [&'static str];
    type Props: TryFromKdlTuple;
    type Values: TryFromKdlTuple;

    fn parse(values: Self::Values, props: Self::Props) -> Self;
}

pub fn parse_node<N>(node: &KdlNode) -> Result<N, Error>
where
    N: NodeShape,
{
    let values = expect_values(node)?;
    let props = expect_props(node, N::PROP_NAMES)?;
    Ok(N::parse(values, props))
}
