syntax on
filetype indent on
filetype plugin on
colorscheme ben

set autoindent
set cursorline
set hlsearch
set modeline
set number
set ruler
set tags=tags
set smarttab
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set backspace=indent,eol,start
set nocindent
set autoindent
set wildmode=longest,list,full
set wildmenu

"map j gj
"map k gk
"map ^ g^
"map $ g$
map gr gT
map m :cnext<CR>
map e :e 
map t :tabe 
map gF :tabe <cfile><CR>
map q Gyy<c-o>p
map Q Gyy<c-o>P
map & *<c-o>
map Xw :wa<cr>
map XX :qa<cr>
map XQ :qa!<cr>
map <c-j> <c-e>
map <c-k> <c-y>
map \ :noh<cr>

" Taken from :help [I
map <F5> [I:let nr = input("Which one: ")<Bar>exe "normal " . nr ."[\t"<CR>

"
" For switching between .cc and .h files.
" Taken from koz.
"
fu! SetSuffix(fn, suffix)
    return substitute(a:fn, "\\.[^.]*$", "." . a:suffix, "")
endfunction

fu! GetOther(fn)
    if a:fn =~ "\\.h$"
        " Attempt to open any WebKit cpp file.
        let s:cpp = SetSuffix(a:fn, "cpp")
        if filereadable(s:cpp)
            return s:cpp
        endif
        " Attempt to open an ObjC file.
        let s:mm = SetSuffix(a:fn, "mm")
        if filereadable(s:mm)
            return s:mm
        endif
        " Default to opening a (possibly new) .cc file.
        return SetSuffix(a:fn, "cc")
    elseif a:fn =~ "\\.cc$" || a:fn =~ "\\.cpp" || a:fn =~ "\\.mm"
        return SetSuffix(a:fn, "h")
    endif
    return 1
endfunction

fu! Switch()
    let s:temp = GetOther(expand("%"))
    if s:temp != 1
        exe ":e " . s:temp
    endif
endfunction

map _ :call Switch()<CR>
