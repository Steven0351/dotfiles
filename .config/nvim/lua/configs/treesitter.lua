-- require'nvim-treesitter.configs'.setup {
--     ensure_installed = "all",
--     highlight = { -- enable highlighting for all file types
--       enable = true, 
--     },
--     incremental_selection = {
--       enable = true,
--       disable = { "cpp", "lua" },
--       keymaps = { -- mappings for incremental selection (visual mappings)
--         init_selection = "gnn",         -- maps in normal mode to init the node/scope selection
--         node_incremental = "grn",       -- increment to the upper named parent
--         scope_incremental = "grc",      -- increment to the upper scope (as defined in locals.scm)
--         node_decremental = "grm",       -- decrement to the previous node
--       }
--     },
--     textobjects = {
--       select = {
--         enable = true,  
--         keymaps = {
--           -- You can use the capture groups defined here:
-- 	  -- https://github.com/nvim-treesitter/nvim-treesitter-textobjects/blob/master/queries/c/textobjects.scm
--           ["af"] = "@function.outer",
--           ["if"] = "@function.inner",
--           ["ab"] = "@block.outer",
--           ["ib"] = "@block.inner",
--           ["as"] = "@statement.outer",
--           ["is"] = "@statement.inner",
--         },
--       },
--       lsp_interop = {
--         enable = true,
--         border = 'none',
--         peek_definition_code = {
--           ["df"] = "@function.outer",
--           ["dF"] = "@class.outer",
--         },
--       },
--     },
-- }
local ts_configs = require 'nvim-treesitter.configs'
ts_configs.setup {
  ensure_installed = 'maintained',
  highlight = { enable = true, use_languagetree = true },
  indent = { enable = false },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = 'gnn',
      node_incremental = 'grn',
      scope_incremental = 'grc',
      node_decremental = 'grm',
    },
  },
  refactor = {
    smart_rename = { enable = true, keymaps = { smart_rename = 'grr' } },
    highlight_definitions = { enable = true },
    -- highlight_current_scope = { enable = true }
  },
  textobjects = {
    select = {
      enable = true,
      keymaps = {
        ['iF'] = {
          python = '(function_definition) @function',
          cpp = '(function_definition) @function',
          c = '(function_definition) @function',
          java = '(method_declaration) @function',
        },
        -- or you use the queries from supported languages with textobjects.scm
        ['af'] = '@function.outer',
        ['if'] = '@function.inner',
        ['aC'] = '@class.outer',
        ['iC'] = '@class.inner',
        ['ac'] = '@conditional.outer',
        ['ic'] = '@conditional.inner',
        ['ae'] = '@block.outer',
        ['ie'] = '@block.inner',
        ['al'] = '@loop.outer',
        ['il'] = '@loop.inner',
        ['is'] = '@statement.inner',
        ['as'] = '@statement.outer',
        ['ad'] = '@comment.outer',
        ['am'] = '@call.outer',
        ['im'] = '@call.inner',
      },
    },
  },
}
