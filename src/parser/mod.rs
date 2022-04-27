mod basic;
mod items;
mod langs;
mod utils;
// short for alias_conn_or_inst
// NOTE: The parsing of these types is closely coupled with "TopItem" and "BaseItem" because of where we
// apply "cut" matters w.r.t the set of items "Connection", "Alias", and "Instance." (and their parsing order)

pub mod ast {
    pub use super::basic::ast::*;
    pub use super::items::ast::*;
    pub use super::langs::ast::*;
    pub use super::utils::SepList1;
}

pub use items::top_level;
