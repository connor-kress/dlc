// TODO: This should be a .dar file
pub static STDLIB: &str = r#"
/**
 * Input/output functions
 */
extern fn printf(fmt: *u8, ...) -> i32;
extern fn scanf(fmt: *u8, ...) -> i32;

/**
 * Memory management functions
 */
extern fn malloc(bytes: u64) -> *void;
extern fn free(ptr: *void) -> void;
extern fn calloc(n: u64, size: u64) -> *void;
extern fn realloc(ptr: *void, new_size: u64) -> *void;

/**
 * String functions
 */
extern fn strlen(s: *u8) -> u64;
extern fn strcmp(a: *u8, b: *u8) -> i32;
extern fn strcpy(dest: *u8, src: *u8) -> *u8;
extern fn strncpy(dest: *u8, src: *u8, num: u64) -> *u8;

/**
 * Memory block functions
 */
extern fn memset(ptr: *void, value: i32, num: u64) -> *void;
extern fn memcpy(dest: *void, src: *void, num: u64) -> *void;
extern fn memmove(dest: *void, src: *void, num: u64) -> *void;
"#;
