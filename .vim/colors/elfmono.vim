function! GetAllGroups()
    " code taken from https://vi.stackexchange.com/a/10456
    let synlist = split(execute('hi'), '\n')
    call filter(synlist, 'v:val =~# "^\\w"')
    call map(synlist, 'split(v:val)[0]')
    return synlist
endfunction

function! DumpAllGroups()
    let synlist = GetAllGroups()
    for item in synlist
        call append(line('$'), printf('%s', item))
    endfor
endfunction

function! DisableHighlightingAllGroups()
    let synlist = GetAllGroups()
    for item in synlist
        try
            exec printf('hi %s NONE', item)
        catch
        endtry
    endfor
endfunction

function! ApplySyntax()
    hi clear
    if (exists('syntax_on'))
        syntax reset
    endif

    call DisableHighlightingAllGroups()
    let g:colors_name = 'elfmono'

    hi Comment ctermfg=brown
    hi Constant cterm=bold
    hi Special cterm=bold
    hi Keyword cterm=bold
    hi Statement cterm=bold
    hi Todo cterm=bold
    hi Pmenu cterm=reverse
    hi Wildmenu cterm=reverse
    hi Underlined cterm=underline
    hi Visual ctermfg=darkmagenta cterm=reverse,bold
    hi IncSearch cterm=reverse
    hi Search cterm=reverse
    hi MatchParen ctermfg=red cterm=underline
    hi StatusLine cterm=underline
    hi StatusLineNC cterm=underline
    hi StatusLineTerm cterm=underline
    hi StatusLineTermNC cterm=underline
endfunction

call ApplySyntax()
