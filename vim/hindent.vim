if exists("g:loaded_hindent") || !executable("hindent")
    finish
endif
let g:loaded_hindent = 1

if !exists("g:hindent_style")
    let g:hindent_style = "fundamental"
endif

if has("autocmd")
  let hindent = "hindent --style " . g:hindent_style
  autocmd FileType haskell setlocal formatexpr=FormatprgLocal(hindent)
endif
