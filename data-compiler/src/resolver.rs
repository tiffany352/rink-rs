use core::fmt;
use core::hash::Hash;

use indexmap::{IndexMap, IndexSet};

pub trait DependencyNode {
    type Id: Hash + Eq + Clone + fmt::Display + fmt::Debug;
    type Intermediate: Clone;
    type Output;

    fn id(&self) -> Self::Id;
    fn dependencies(&self) -> Vec<Self::Id>;
    fn process(&self, dependencies: Vec<Self::Intermediate>) -> (Self::Intermediate, Self::Output);
}

#[derive(thiserror::Error, Clone, PartialEq, Eq, Debug)]
pub enum DependencyError<T>
where
    T: fmt::Display + fmt::Debug,
{
    #[error("`{parent}` references non-existent node `{child}`")]
    IdDoesNotExist { parent: T, child: T },
    #[error("circular dependency chain in {0}")]
    CircularDependency(DependencyPath<T>),
    #[error("dependency chain is impossibly deep")]
    DepthLimitExceeded,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DependencyPath<T>(Vec<T>);

impl<T> fmt::Display for DependencyPath<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, item) in self.0.iter().enumerate() {
            if i != 0 {
                write!(f, " -> ")?;
            }
            write!(f, "`{}`", item)?;
        }
        Ok(())
    }
}

#[derive(thiserror::Error, Clone, PartialEq, Eq, Debug)]
pub struct DependencyErrors<T>(Vec<DependencyError<T>>)
where
    T: fmt::Display + fmt::Debug;

impl<T> fmt::Display for DependencyErrors<T>
where
    T: fmt::Display + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "multiple errors:")?;
        for error in &self.0 {
            write!(f, "\n- {}", error)?;
        }
        Ok(())
    }
}

struct Dependency<T>
where
    T: DependencyNode,
{
    value: T,
    dependencies: Vec<T::Id>,
}

fn visit<'a, T>(
    id: &'a T::Id,
    value: &'a Dependency<T>,
    lookup: &'a IndexMap<T::Id, Dependency<T>>,
    outputs: &mut IndexMap<T::Id, Option<(T::Intermediate, T::Output)>>,
    errors: &mut Vec<DependencyError<T::Id>>,
    stack: &mut IndexSet<&'a T::Id>,
    max_depth: usize,
) -> Option<T::Intermediate>
where
    T: DependencyNode,
{
    if max_depth == 0 {
        errors.push(DependencyError::DepthLimitExceeded);
        return None;
    }

    // Already visited
    if let Some(ordered) = outputs.get(id) {
        return ordered.as_ref().map(|v| v.0.clone());
    }

    if !stack.insert(id) {
        let index = stack.get_index_of(id).unwrap();
        errors.push(DependencyError::CircularDependency(DependencyPath(
            stack.iter().skip(index).cloned().cloned().collect(),
        )));
        return None;
    }
    let mut intermediates = vec![];
    let mut failure = false;
    for dep in &value.dependencies {
        if let Some(dep_deps) = lookup.get(dep) {
            if let Some(intermediate) =
                visit(dep, dep_deps, lookup, outputs, errors, stack, max_depth - 1)
            {
                intermediates.push(intermediate);
            } else {
                failure = true;
            }
        } else {
            errors.push(DependencyError::IdDoesNotExist {
                parent: id.clone(),
                child: dep.clone(),
            });
        }
    }

    let result = if failure {
        None
    } else {
        Some(value.value.process(intermediates))
    };

    let ret = result.as_ref().map(|v| v.0.clone());
    outputs.insert(id.clone(), result);
    stack.pop();
    ret
}

pub fn resolve_dependencies<T>(input: Vec<T>) -> Result<Vec<T::Output>, DependencyErrors<T::Id>>
where
    T: DependencyNode,
{
    let lookup = input
        .into_iter()
        .map(|dep| {
            (
                dep.id(),
                Dependency {
                    dependencies: dep.dependencies(),
                    value: dep,
                },
            )
        })
        .collect::<IndexMap<T::Id, Dependency<T>>>();
    let mut ordering = IndexMap::<T::Id, Option<(T::Intermediate, T::Output)>>::new();
    let mut errors = vec![];

    let mut stack = IndexSet::new();
    for (id, deps) in &lookup {
        visit(
            id,
            deps,
            &lookup,
            &mut ordering,
            &mut errors,
            &mut stack,
            lookup.len(),
        );
    }

    if errors.is_empty() {
        let outputs = ordering
            .into_iter()
            .map(|(_, ord)| ord.unwrap().1)
            .collect();
        Ok(outputs)
    } else {
        Err(DependencyErrors(errors))
    }
}
