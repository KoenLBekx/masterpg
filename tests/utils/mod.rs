use std::ffi::OsString;
use std::fs::{create_dir, remove_dir_all};
use std::path::Path;
use std::str::FromStr;

pub const TESTFILES_DIR: &str = "./tests/playground";

pub fn ensure_playground(remove_first: bool) -> OsString {
    let dir_path = Path::new(&TESTFILES_DIR);

    if remove_first && dir_path.is_dir() {
       remove_dir_all(&dir_path).unwrap(); 
    }

    if !dir_path.is_dir() {
        create_dir(&dir_path).unwrap();
    }

    OsString::from_str(TESTFILES_DIR).unwrap()
}
