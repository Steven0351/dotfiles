require('configs.lsp')
local opt = vim.opt
local cmd = vim.cmd
local g   = vim.g

g.nocompatible     = true
g.nobackup         = true
g.noswapfile       = true
g.lightline        = { colorscheme = 'nord' }

opt.incsearch      = true
opt.wildmenu       = true
opt.hidden         = true
opt.number         = true
opt.relativenumber = true
opt.clipboard      = 'unnamedplus'
opt.expandtab      = true
opt.smarttab       = true
opt.smartindent    = true
opt.shiftwidth     = 4
opt.tabstop        = 4
opt.splitbelow     = true
opt.splitright     = true
opt.wrap           = true
opt.termguicolors  = true

vim.o.completeopt = "menuone,noselect"

cmd 'syntax enable'
cmd 'filetype off'
cmd 'filetype plugin indent on'
cmd 'colorscheme nord'
cmd [[au VimEnter * highlight LineNr  ctermfg=3 ctermbg=none cterm=none]]
cmd [[au VimEnter * highlight Comment ctermfg=5 ctermbg=none cterm=italic]]
cmd [[command! PackerInstall packadd packer.nvim | lua require('plugins').install()]]
cmd [[command! PackerUpdate packadd packer.nvim | lua require('plugins').update()]]
cmd [[command! PackerSync packadd packer.nvim | lua require('plugins').sync()]]
cmd [[command! PackerClean packadd packer.nvim | lua require('plugins').clean()]]
cmd [[command! PackerCompile packadd packer.nvim | lua require('plugins').compile()]]
