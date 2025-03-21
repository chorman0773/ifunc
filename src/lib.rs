//! Indirect Function (IFUNC/Symbol Resolver) Support in Rust as a proc-macro
//!
//! ## How to Use
//!
//! To use [`ifunc`][crate], simply apply the [`indirect_func`] attribute to your function.
//! Then modify the body of the function such that it returns a captureless closure (or a function item/pointer) to the appropriate body (the resolver),
//!  rather than just directly including the body of the function.  The signature (parameters and return values) is of the function being called
//!
//! ```rust
//! # use ifunc::indirect_func;
//! #[indirect_func]
//! pub fn foo() -> i32 {
//!   || 0
//! }
//! # fn main() {
//! assert_eq!(foo(), 0)
//! # }
//! ```
//!
//! You can do control flow in the body (though you can't use the values of the parameters or capture local variables with the closure). For example:
//!
//! ```rust
//! # use ifunc::indirect_func;
//!
//! #[indirect_func]
//! pub fn bar() -> i32 {
//!    if FOO {
//!        || 1
//!    } else {
//!        || 2
//!    }
//! }
//! static FOO: bool = true;
//! # fn main() {
//! assert_eq!(bar(), 1)
//! # }
//! ```
//!
//! The resolver is run once, before the first time the function is called.
//!
//! The body is typechecked so that the returned signatures match the function.
//!
//! ```rust,compile_fail
//! # use ifunc::indirect_func;
//! #[indirect_func]
//! pub fn baz() -> i32 {
//!     || 1.0 // Error: Expected `i32`, got `f32`
//! }
//! ```
//!
//! It is also typechecked so that the closures do not capture anything:
//!
//! ```rust,compile_fail
//! # use ifunc::indirect_func;
//! # pub fn get_random_number() -> i32 {
//! # 4 // https://xkcd.com/221/
//! # }
//! #[indirect_func]
//! pub fn baz() -> i32 {
//!     let val = get_random_number();
//!     || val
//! }
//! ```
//!
//! ## Target Function
//!
//! When you apply the [`indirect_func`] attribute to a function, there are now two possible functions that can be referred to.
//! We call these respectively the "Target Function" and the "Resolver".
//!
//! When writing an [`indirect_func`] the *signature* of the function is that of the Target Function, and the *body* is that of the Resolver.
//!
//! The resolver is an unnameable function with signature `extern "C" fn () -> F`, where `F` is the fn-pointer type corresponding to the signature of the target function.
//!
//! If the resolver (not the target function) panics, the process aborts. If you intend calls to the target function to immediately panic, instead resolve to a function that unconditionally panics.
//!
//! ## Why to use it
//!
//! The main reason to use this is if you want to test properties of the system before deciding what function to call.
//! For example, a test on target features to dispatch based on the runtime CPU support.
//!
//! ## Target Support
//!
//! This crate presently only supports ELF and Mach-O targets.
//! For ELF it requires support of `STT_GNU_IFUNC` and `R_*_IRELATIVE`. Currently this is hardcoded to linux (and Lilium) targets.
//! Mach-O is also specifically supported only for apple (Darwin) targets, such as MacOS or iOS.
//!
//! If you're writing for a custom target, or for a baremetal target, but know you have access to the appropriate support, you can set `--cfg unsafe_ifunc_allow_target`.
//! By setting this variable, you assert to [`ifunc`][crate] that the behaviour is correct. By default, it assumes the target is using ELF.
//! If you are on a Mach-O target that is not Apple, you also need to set `--cfg unsafe_ifunc_binary_format="mach-o"` (You can also explicitly set this cfg to `elf` or unset it).
//!
//! Note that these are bare `cfg` values, not cargo features. You must set these in `RUSTFLAGS` when building, rather than with `Cargo.toml`. Also note that they must be set when building the `ifunc` crate.
//!
//! ## Export Names
//!
//! One use for the [`indirect_func`] attribute is for system libraries. Therefore, you can apply `#[unsafe(no_mangle)]` and `#[unsafe(export_name)]` to the function.
//! This has the desired behaviour, that the exported function can be called by the appropriate non-mangled name.
//!
//! Otherwise, an internal, mangled name is used. The name will be reasonably readable in debugging, but you cannot rely on the precise form of the name.
//! (Note that collisions are unlikely, but possible in theory)
//!
//! Note that the default abi of the resolved function is the same as any other function, `extern "Rust"`. Therefore, it cannot be called from outside of Rust.
//! If you need to define a system routine using [`indirect_func`], you still have to make the function `extern "C"` (but see [Limitations](#limitations) below)
//!
//! Due to limitations in macros, using an `#[unsafe]` attribute is required for `#[no_mangle]` and `#[export_name]` (even when compiling for editions prior to 2024)
//!
//! ## Safety
//!
//! If the function marked as [`indirect_func`] is `unsafe`, then the resulting function will be unsafe.
//! However, the resolver will not be marked as unsafe.
//! Therefore, to use unsafe operations within the body of the resolver itself, you need to explicitly add an `unsafe` block.
//! You are responsible for ensure the preconditions of the block is met at any point the resolver may be called (which can include before `main`).
//! In particular, you cannot solely rely on any precondition of the target function.
//!
//! ## Attributes
//!
//! Attributes applied to the function are divided into three categories:
//! * Attributes that apply to the target function,
//! * Attributes that apply to the resolver, and
//! * Attributes that apply to both.
//!
//! The attributes `#[export_name]` and `#[no_mangle]` are treated specially (they are handled as-if they apply to the target function).
//!
//! The following attributes apply to the target:
//! * `#[doc]` attributes
//! * `#[deprecated]`
//! * `#[must_use]`
//! * `#[cold]`
//!
//! The following attributes apply to both the target and the resolver:
//! * `#[cfg]`
//! * Lint attributes (allow, warn, expect, etc.). Note that `expect` may not have the desired behaviour when applied to an [`indirect_func`].
//!
//!
//! The following attributes apply to the resolver only:
//! * `#[target_feature]`. Note that this is equivalent to adding `-C target-feature="+<feature-list>"` to the crate flags.
//! * `#[unsafe(link_section)]` (See limitations for further details).
//!
//! Additionally, any attribute not in any of the above lists will default to being applied to the resolver only. This includes tool attributes, and macro attributes.
//! Attributes
//!
//! ## Limitations
//!
//! The following limitations are noted. Some may be fixed in a future release:
//! * It is possible for `#[indirect_func]` to be run before `main` is entered. Some parts of `libstd` may not function properly from inside the resolver, This makes the `ifunc_attribute` `unsafe` (but there is no way to communicate this).
//! * `#[inline]` does not have any effect on an #\[[`indirect_func`]\]. The attribute can be applied but is silently ignored.
//!    * This may be fixed in the future with cooperation from the compiler. Such support will likely require the use of LTO.
//! * The body of the resolver function is not currently modified in any way. Therefore, return values of the resolver must match the signature of the target function. In particular:
//!    * Parameters must be declared both by the function and the closure,
//!    * Closures in particular require the abi of the function to be `"Rust"`, and therefore an explicit function must be used if you want to return another abi such as `extern "C"`.
//! * You need to wrap `#[target_feature]` functions in a wrapper to use.
//! * Due to the way the resolver is implemented, if the ABI of the function is not `extern "Rust"`, then non-FFI safe types will trigger an `improper_ctypes` lint *not* `improper_ctypes_definitions`
//!   This may be important if the type is erroneously considered non-FFI safe (a thin raw pointer that is only used as a pointer by FFI).
//! * `#[track_caller]` is presently a compile-error when applied to an [`indirect_func`].
//! * `#[link_section]` only affects the actualy resolver function. Some code is generated in addition to the resolver, which will presently be placed in the default section for program code (typically `.text`).
//! * `#[cfg_attr]` cannot be used with `#[no_mangle]` or `#[export_name]`. Otherwise, it is supported as normal (and propagates to the internal attributes).

#![cfg_attr(not(test), no_std)]

/// Marks a function as being "indirect" and requires the body to resolve the function to be called.
///
/// See the [crate-level][`crate`] documentation for details
///
/// # Safety
/// Because of how indirect function resolvers work, you must guarantee two properties (these apply to the resolver itself, not to the target function):
/// 1. The resolver body must not be reentrant. IE. it must not use the value of the symbol it is trying to resolve (to call it or to convert it to a function pointer),
/// 2. You must not rely on the timing of the resolver being called (except that it will be called at the latest immediately before calling the function itself).
///  In particular, you must not rely on features of the standard library that may not work before `main`.
pub use ifunc_attribute::indirect_func;

#[doc(hidden)]
pub use core as _core;

#[cfg(not(any(
    doc,
    target_os = "linux",
    target_os = "macos",
    target_os = "ios",
    target_os = "tvos",
    target_os = "watchos",
    target_os = "visionos",
    target_os = "lilium",
    unsafe_ifunc_allow_target
)))]
core::compile_error!("Requires an ELF (with GNU_IFUNC support) or Mach-O target to compile");

/// A primitive macro that allows creation of a resolver for `$sym` that calls `$resolver`.
/// This is typically used via [`indirect_func`], however it can also be called directly.
///
/// `$resolver` must have the signature `extern "C" fn() -> F` where `F` is the signature of the to-be-resolved function.
/// `$sym` is the stringified name of the symbol you want to define.
///
/// # Safety
/// The resolver is run with the same timing as [`indirect_func`] is. Therefore, the constraints on a use if [`indirect_func`] also apply to the specified resolver.
/// Additionally, you are responsible for ensuring the type signature of `$resolver` matches. No compile-time checks are performed
#[macro_export]
#[cfg(any(
    target_os = "macos",
    target_os = "ios",
    target_os = "tvos",
    target_os = "watchos",
    target_os = "visionos",
    unsafe_ifunc_binary_format = "mach-o"
))]
macro_rules! create_resolver {
    ($sym:literal, $resolver:ident) => {
        $crate::_core::arch::global_asm! {
            $crate::_core::concat!(".symbol_resolver ", $sym),
            "jmp {resolver}",
            resolver = sym $resolver
        }
    };
}

/// A primitive macro that allows creation of a resolver for `$sym` that calls `$resolver`.
/// This is typically used via [`indirect_func`], however it can also be called directly.
///
/// `$resolver` must have the signature `extern "C" fn() -> F` where `F` is the signature of the to-be-resolved function.
/// `$sym` is the stringified name of the symbol you want to define.
///
/// # Safety
/// The resolver is run with the same timing as [`indirect_func`] is. Therefore, the constraints on a use if [`indirect_func`] also apply to the specified resolver.
/// Additionally, you are responsible for ensuring the type signature of `$resolver` matches. No compile-time checks are performed
#[macro_export]
#[cfg(not(any(
    target_os = "macos",
    target_os = "ios",
    target_os = "tvos",
    target_os = "watchos",
    target_os = "visionos",
    unsafe_ifunc_binary_format = "mach-o"
)))]
macro_rules! create_resolver {
    ($sym:literal, $resolver:ident) => {
        $crate::_core::arch::global_asm! {
            $crate::_core::concat!(".type ", $sym, ", STT_GNU_IFUNC"),
            $crate::_core::concat!(".global ", $sym),
            $crate::_core::concat!($sym,":"),
            "jmp {resolver}",
            resolver = sym $resolver
        }
    };
}

#[cfg(test)]
extern crate self as ifunc;

#[cfg(test)]
mod test {

    use ifunc_attribute::indirect_func;

    #[indirect_func]
    fn foo() -> String {
        || String::from("foo")
    }

    #[test]
    fn foo_test() {
        assert_eq!(foo(), "foo")
    }

    #[indirect_func]
    fn bar() -> i32 {
        || 0
    }

    #[test]
    fn bar_test() {
        assert_eq!(bar(), 0)
    }
}
