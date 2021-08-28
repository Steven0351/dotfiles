local utils = {}
local api = vim.api
function utils.isLinux()
    fh,err = io.popen("uname -o 2>/dev/null","r")
    if fh then
        osname = fh:read()
        return string.match(osname, 'Linux') 
    end
    return false
end

function utils.nnoremap(lhs, rhs, opts)
    local options = { noremap = true, silent = true }
    if opts then options = vim.tbl_extend('force', options, opts) end
    api.nvim_set_keymap('n', lhs, rhs, options)
end

function utils.map(lhs, rhs, opts)
    local options = { noremap = true, silent = true }
    if opts then options = vim.tbl_extend('force', options, opts) end
    api.nvim_set_keymap('', lhs, rhs, options)
end

function utils.imap(lhs, rhs, opts)
    local options = { noremap = true, silent = true }
    if opts then options = vim.tbl_extend('force', options, opts) end
    api.nvim_set_keymap('i', lhs, rhs, options)
end

return utils
