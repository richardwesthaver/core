//! macs.rs --- SEXP Macro support
use crate::form::Form;
use std::collections::HashMap;
pub type Macro = dyn FnMut(&mut Form);
pub type MacroObject = Box<Macro>;
pub type ReadTable = HashMap<char, Box<Macro>>; // could use indexmap here..
pub type WriteTable = Vec<(bool, Box<Macro>)>;
