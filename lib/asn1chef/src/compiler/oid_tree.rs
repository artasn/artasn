use lazy_static::lazy_static;
use serde::Deserialize;

use crate::values::Oid;

use super::parser::{AstElement, Error, ErrorKind, Result};

#[derive(Deserialize)]
pub struct OidTreeNode {
    pub node: u64,
    pub name: &'static str,
    pub aliases: Option<Vec<&'static str>>,
    pub children: Option<Vec<OidTreeNode>>,
}

impl OidTreeNode {
    pub fn lookup_node<'a>(&'a self, name: &str) -> Option<&'a OidTreeNode> {
        if let Some(children) = &self.children {
            for child_node in children {
                if child_node.name == name
                    || child_node
                        .aliases
                        .as_ref()
                        .map(|aliases| aliases.contains(&name))
                        .unwrap_or(false)
                {
                    return Some(child_node);
                }
            }
        }

        None
    }

    pub fn get_node<'a>(&'a self, num: u64) -> Option<&'a OidTreeNode> {
        if let Some(children) = &self.children {
            for child_node in children {
                if child_node.node == num {
                    return Some(child_node);
                }
            }
        }

        None
    }
}

lazy_static! {
    static ref ROOT_NODES: Vec<OidTreeNode> =
        serde_json::from_str(include_str!("./oid_tree.json")).unwrap();
}

pub enum OidTreeNodeSearch {
    Name(AstElement<String>),
    Number(u64),
}

pub fn search(searches: Vec<OidTreeNodeSearch>) -> Result<Oid> {
    if searches.len() == 0 {
        panic!("oid has no nodes");
    }

    let mut oid = Vec::new();
    let mut current_node = match &searches[0] {
        OidTreeNodeSearch::Name(name) => {
            let root = lookup_root_node(&name.element).ok_or_else(|| Error {
                kind: ErrorKind::Ast(format!(
                    "unknown named root oid component '{}'",
                    name.element,
                )),
                loc: name.loc,
            })?;
            oid.push(root.node);
            Some(root)
        }
        OidTreeNodeSearch::Number(num) => {
            let num = *num;
            oid.push(num);
            get_root_node(num)
        }
    };
    for i in 1..searches.len() {
        let search = &searches[i];
        match search {
            OidTreeNodeSearch::Name(name) => {
                let next_node = (match current_node {
                    Some(current_node) => current_node.lookup_node(&name.element),
                    None => None,
                })
                .ok_or_else(|| Error {
                    kind: ErrorKind::Ast(
                        format!("unknown named oid component '{}'", name.element,),
                    ),
                    loc: name.loc,
                })?;
                oid.push(next_node.node);
                current_node = Some(next_node);
            }
            OidTreeNodeSearch::Number(num) => {
                let num = *num;
                let next_node = match current_node {
                    Some(current_node) => current_node.get_node(num),
                    None => None,
                };
                oid.push(num);
                current_node = next_node;
            }
        }
    }

    Ok(Oid(oid))
}

pub fn lookup_root_node<'a>(name: &str) -> Option<&'a OidTreeNode> {
    for root_node in ROOT_NODES.iter() {
        if root_node.name == name
            || root_node
                .aliases
                .as_ref()
                .map(|aliases| aliases.contains(&name))
                .unwrap_or(false)
        {
            return Some(root_node);
        }
    }

    None
}

pub fn get_root_node<'a>(num: u64) -> Option<&'a OidTreeNode> {
    for root_node in ROOT_NODES.iter() {
        if root_node.node == num {
            return Some(root_node);
        }
    }

    None
}
