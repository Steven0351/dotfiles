-- Packer config comes from author of packer 
-- https://github.com/wbthomason/dotfiles/blob/linux/neovim/.config/nvim/lua/plugins.lua
local packer = nil
local function init()
  if packer == nil then
    packer = require 'packer'
    packer.init { disable_commands = true }
  end

  local use = packer.use
  packer.reset()
  
  -- Let packer manage itself
  use 'wbthomason/packer.nvim'
   
  -- Configs for LSP
  use {
    'neovim/nvim-lspconfig',
    'onsails/lspkind-nvim', -- Symbols for LSP preview
    'nvim-lua/lsp-status.nvim', -- Shows LSP status in statusline
    'folke/trouble.nvim', -- LSP Diagnostics
    'ray-x/lsp_signature.nvim', -- Function signatures
    'kosayoda/nvim-lightbulb', -- VS Code like lightbulb suggestions
  }

  -- use {
  --   'glepnir/lspsaga.nvim',
  --   requires = 'neovim/nvim-lspconfig'
  -- }

  use {
    "hrsh7th/nvim-compe",
    event = "InsertEnter",
    config = [[require('configs.compe')]]
  }

  use {
    "hrsh7th/vim-vsnip",
    -- wants = "friendly-snippets",
    event = "InsertEnter",
  }

  -- use {
  --   "rafamadriz/friendly-snippets",
  --   event = "InsertCharPre",
  -- }

  -- use {
  --   'neoclide/coc.nvim',
  --   branch = 'release'
  -- }
  -- -- Pretty symbols
  use {
    'kyazdani42/nvim-web-devicons',
    config = function()
      require("nvim-web-devicons").setup { default = true }
    end,
  }
  
  -- A File Explorer For Neovim Written In Lua
  use {
    'kyazdani42/nvim-tree.lua',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function()
      require("configs.nvimtree").config()
    end
  }

  -- Searching and finding
  use {
    'nvim-telescope/telescope.nvim',
    requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}},
    config = [[require('configs.telescope')]]
  }
  
  -- Media preview
  use {
    'nvim-telescope/telescope-media-files.nvim',
    requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}, {'nvim-telescope/telescope.nvim'}}
  }

  -- Syntax Highlighting
  use {
    'nvim-treesitter/nvim-treesitter',
    branch = '0.5-compat',
    run = ':TSUpdate',
    config = [[require('configs.treesitter')]]
  }

  -- Code navigation
  
  use {
    'nvim-treesitter/nvim-treesitter-refactor',
    requires = 'nvim-treesitter/nvim-treesitter'
  }

  use {
    'nvim-treesitter/nvim-treesitter-textobjects',
    branch = '0.5-compat',
    requires = 'nvim-treesitter/nvim-treesitter'
  }
  

  -- Status Line
  use 'itchyny/lightline.vim'
  
  -- Change surroundings marks (e.g. "asdf" to 'asdf') 
  use 'tpope/vim-surround'

  -- Nord theme
  use 'arcticicestudio/nord-vim'

  -- Autocomplete closing delimiters
  use 'raimondi/delimitmate'

  -- Show LSP TODOs/HACKs in pretty list
  use {
    'folke/todo-comments.nvim',
    requires = 'nvim-lua/plenary.nvim',
    config = [[require('configs.todocomments')]]
  }

  -- Indentation tracking
  use {
    'lukas-reineke/indent-blankline.nvim',
    config = [[require('configs.indentblankline')]]
  }

  -- Git signs in the gutter
  use {
    'lewis6991/gitsigns.nvim',
    requires = 'nvim-lua/plenary.nvim',
    config = [[require('configs.gitsigns')]],
    event = 'BufRead'
  }

  -- Highlights color codes with their rendered color
  use {
    'norcalli/nvim-colorizer.lua',
    config = [[require('configs.colorizer')]],
    ft = {}
  }

  -- Commenting code
  use {
    'b3nj5m1n/kommentary',
    config = [[require('configs.kommentary')]],
  }

  -- Tab bar
  use {
    'romgrk/barbar.nvim',
    requires = {'kyazdani42/nvim-web-devicons'},
    config = function()
      require("configs.barbar")
    end
  }
end

local plugins = setmetatable({}, {
 __index = function(_, key)
   init()
   return packer[key]
 end,
})

return plugins
