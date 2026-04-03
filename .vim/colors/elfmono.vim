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

function! ClearSyntax()
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

    call ClearSyntax()
    let g:colors_name = 'elfmono'

    let table = {
                \ 'Title': 'bold',
                \ 'Error': 'bold,reverse',
                \ 'MatchParen': 'bold,underline',
                \ 'Search': 'reverse',
                \ 'IncSearch': 'reverse',
                \ 'SpellBad': 'underline',
                \ 'SpellCap': 'underline',
                \ 'SpellLocal': 'underline',
                \ 'SpellRare': 'underline',
                \ 'Visual': 'reverse',
                \ 'String': 'bold',
                \ 'Todo': 'bold,reverse',
                \ 'Comment': 'bold',
                \ 'SpecialComment': 'bold',
                \ 'WildMenu': 'reverse',
                \ 'Underlined': 'underline',
                \ 'Pmenu': 'reverse'
                \ }

    for [group, style] in items(table)
        exec printf('hi %s cterm=%s term=%s', group, style, style)
    endfor
endfunction

call ApplySyntax()
