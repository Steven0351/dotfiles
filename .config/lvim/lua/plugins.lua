lvim.plugins = {
  {
    "ray-x/lsp_signature.nvim",
    config = function() require"lsp_signature".on_attach() end,
    event = "InsertEnter"
  },
  {
    'shaunsingh/nord.nvim'
  }
}
