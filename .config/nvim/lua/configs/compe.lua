-- require'compe'.setup {
--     active = true,
--     on_config_done = nil,
--     autocomplete = true,
--     debug = false,
--     min_length = 1,
--     preselect = "enable",
--     throttle_time = 80,
--     source_timeout = 200,
--     incomplete_delay = 400,
--     max_abbr_width = 100,
--     max_kind_width = 100,
--     max_menu_width = 100,
--     documentation = {
--       border = "single",
--       winhighlight = "NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder",
--       max_width = 120,
--       min_width = 60,
--       max_height = math.floor(vim.o.lines * 0.3),
--       min_height = 1,
--     },

--     source = {
--       path = { kind = "   (Path)" },
--       buffer = { kind = "   (Buffer)" },
--       calc = { kind = "   (Calc)" },
--       vsnip = { kind = "   (Snippet)" },
--       nvim_lsp = { kind = "   (LSP)" },
--       nvim_lua = true,
--       spell = false,
--       tags = false,
--       emoji = { kind = " ﲃ  (Emoji)", filetypes = { "markdown", "text" } },
--       -- for emoji press : (idk if that in compe tho)
--     },

--     keymap = {
--       values = {
--         insert_mode = {
--           ["<Tab>"] = { 'pumvisible() ? "<C-n>" : "<Tab>"', { silent = true, noremap = true, expr = true } },
--           ["<S-Tab>"] = { 'pumvisible() ? "<C-p>" : "<S-Tab>"', { silent = true, noremap = true, expr = true } },
--           ["<C-Space>"] = { "compe#complete()", { silent = true, noremap = true, expr = true } },
--           ["<C-e>"] = { "compe#close('<C-e>')", { silent = true, noremap = true, expr = true } },
--           ["<C-f>"] = { "compe#scroll({ 'delta': +4 })", { silent = true, noremap = true, expr = true } },
--           ["<C-d>"] = { "compe#scroll({ 'delta': -4 })", { silent = true, noremap = true, expr = true } },
--         },
--       },
--       opts = {
--         insert_mode = { noremap = true, silent = true, expr = true },
--       },
--     },
-- }

-- vim.opt.shortmess:append('c')
local imap = require('utils').imap
vim.g.loaded_compe_treesitter = true
vim.g.loaded_compe_snippets_nvim = true
vim.g.loaded_compe_spell = true
vim.g.loaded_compe_tags = true
vim.g.loaded_compe_ultisnips = true
vim.g.loaded_compe_vim_lsc = true
vim.g.loaded_compe_vim_lsp = true

require('compe').setup {
  enabled = true,
  autocomplete = true,
  debug = false,
  min_length = 1,
  preselect = 'always',
  documentation = {border = 'single'},
  source = {path = true, buffer = true, nvim_lsp = true, nvim_lua = true, vsnip = true}
}

local opts = {noremap = true, silent = true, expr = true}
imap('<c-c>', [[compe#complete()]], opts)
imap('<cr>', [[compe#confirm('<cr>')]], opts)
imap('<c-e>', [[compe#close('<c-e>')]], opts)
