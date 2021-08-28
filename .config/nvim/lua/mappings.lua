local api = vim.api
local nnoremap = require('utils').nnoremap

-- Toggle nvim-tree. Custom command needed to adjust barbar offset.
nnoremap('<C-n>', [[<cmd>lua require('tree').toggle_tree()<cr>]])

-- Barbar navigation
nnoremap('<A-k>', ':BufferPrevious<cr>')
nnoremap('<A-j>', ':BufferNext<cr>')
nnoremap('<A-c>', ':BufferClose<cr>')

-- map <A-1> through <A-8> to :BufferGoto <buffer_number> 
for i = 1, 8, 1 do
    local index = tostring(i)
    local hotkey = string.format('<A-%d>', i)
    local action = string.format(':BufferGoto %d<cr>', i)
    nnoremap(hotkey, action)
end

nnoremap('<A-9>', ':BufferLast<cr>')

-- Split navigation
nnoremap('<S-H>', '<C-w>h<cr>')
nnoremap('<S-J>', '<C-w>j<cr>')
nnoremap('<S-K>', '<C-w>k<cr>')
nnoremap('<S-L>', '<C-w>l<cr>')

-- LSP Saga
-- noremap('K', ':Lspsaga lsp_finder<cr>')

nnoremap('<A-s>', ':w<cr>') -- Save current buffer
nnoremap('p', ':pu<cr>')    -- Make p paste to newline
