/*

    Copyright (C) 2026  Stevens Benavides

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Fields, parse_macro_input};

#[proc_macro_derive(CompilationIssueCodes)]
pub fn derive_compilation_issue_codes(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input as DeriveInput);
    let name: syn::Ident = input.ident;

    let Data::Enum(data) = input.data else {
        return syn::Error::new_spanned(name, "CompilationIssueCodes requires an enum")
            .to_compile_error()
            .into();
    };

    let mut warnings: Vec<syn::Ident> = Vec::new();
    let mut errors: Vec<syn::Ident> = Vec::new();

    for variant in data.variants {
        if !matches!(variant.fields, Fields::Unit) {
            return syn::Error::new_spanned(
                variant,
                "CompilationIssueCodes only supports unit variants",
            )
            .to_compile_error()
            .into();
        }

        let identifier: syn::Ident = variant.ident;

        if identifier.to_string().starts_with('W') {
            warnings.push(identifier);
        } else {
            errors.push(identifier);
        }
    }

    quote! {
        impl #name {
            pub const ALL_WARNING_CODES: &'static [Self] = &[
                #(Self::#warnings),*
            ];

            pub const fn is_warning(self) -> bool {
                match self {
                    #(Self::#warnings => true,)*
                    #(Self::#errors => false,)*
                }
            }
        }
    }
    .into()
}
