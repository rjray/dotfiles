" Config settings
set nocompatible
set incsearch
set autoindent
set history=1000
set shiftwidth=4
set tabstop=4
set expandtab
set viminfo='1000,f1,<500
set nohlsearch
set grepprg=ack
set grepformat=%f:%l:%m
set backspace=start,indent,eol
set numberwidth=4
set hidden
set showcmd
set fileformat=unix
set wildmenu
set wildmode=list:longest
set ignorecase 
set smartcase
set title
set scrolloff=3
set visualbell
set background=light

" Variable assignments
let mapleader = ","
let MRU_Max_Entries = 25
let MRU_Exclude_Files = '^/tmp/.*\|^/var/tmp/.*'
let MRU_Window_Height = 15
let g:fuzzy_ignore = "*.log"
let g:fuzzy_matching_limit = 70

" Key-mappings
map Q gq
map <leader>t :FuzzyFinderTextMate<cr>
map <leader>b :FuzzyFinderBuffer<cr>
map <C-n> :wn<CR>
nmap <C-n><C-n> :set invnumber<cr>
noremap <Leader>w <C-W><C-W>:res<cr>
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>
nnoremap ' `
nnoremap ` '
map H ^
map L $
map <C-Right> :bnext<CR>
imap <C-Right> <Esc>:bnext<CR>
map <C-Left> :bprev<CR>
imap <C-Left> <Esc>:bprev<CR>
map <C-Del> :bd<CR>
map <F2> :NERDTreeToggle<CR>

" Enable file-type detection, per-filetype plugins, and per-filetype indent
filetype plugin indent on

" Function declarations
function! PerlMappings()
    noremap <buffer> ,cv :call Coverage()<cr>
    noremap K :!perldoc <cword> <bar><bar> perldoc -f <cword><cr>
    if $HOST == "rjray"
        nmap <C-F6> :%!perltidy --profile=.perltidyrc-netapp<cr>
    else
        nmap <C-F6> :%!perltidy<cr>
    endif
endfunction

function! PerlTestMappings()
    noremap <buffer> ,t :!prove -vl --norc %<CR>
endfunction

function! Coverage()
    let filename = bufname('%')
    if match(filename, '\.t$') > -1
        execute '!covered by --test_file="'. filename .'"'
    else
        execute '!covered covering --source_file="'. filename .'"'
    end
endfunction

function! XMLMappings()
    set shiftwidth=2 tabstop=2 formatoptions=t encoding=utf-8 whichwrap=<,>,h,l
    noremap <leader>xf :%!xmllint --format %<cr>
    noremap <leader>xp :call Xpath()<cr>
endfunction

function! Xpath()
    let filename = bufname("%")
    let xpath    = input("Enter xpath expression: ")
    let command  = "xpath '" . filename . "' '" . xpath . "'"
    echo system(command)
endfunction

" Say a message
function! Say(msg)
    echohl IncSearch
    echo a:msg
    echohl None
endfunction

" Copy full buffer to OS clipboard.
function! CopyAll()
    normal mzggVG"+y'z
    call Say("Copied.")
endfunction
command A call CopyAll()

" Delete buffer contents and Paste from OS clipboard.
function! PasteFromClipboard()
    normal ggVGd"+p1Gdd
    call Say("Pasted.")
endfunction
command B call PasteFromClipboard()

" Auto-commands based on file-type and/or buffer life-cycle
au! FileType gitcommit setlocal textwidth=79 noexpandtab 
au! FileType java   set shiftwidth=4 tabstop=4
au! FileType perl          :call PerlMappings()
au! FileType xml           :call XMLMappings()
au! FileType xslt          :call XMLMappings()
au! BufRead,BufNewFile *.t :call PerlTestMappings()
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
au! FileType html  set shiftwidth=2 tabstop=2 formatoptions=t whichwrap=<,>,h,l
au FileType make  set noexpandtab
au FileType text setlocal textwidth=79

" Conditional stuff
if has("gui_running")     " Running under gvim/GUI
    set nowrap number guioptions-=T lines=62 columns=85 guifont=Courier\ New,Courier\ 10
    winpos 117 27
    au VimEnter * source ~/.gvimsession
    au VimLeave * mksession! ~/.gvimsession
    syntax enable
endif
if &t_Co > 1              " If the term/UI supports multi colors
    syntax enable
endif

" Explicit sourcing/loading of external files (other than those under ~/.vim)
runtime macros/matchit.vim
