return {
  "nvim-telescope/telescope.nvim",
  opts = {
    keys = {
      { "<leader>,", false },
    },
    pickers = {
      live_grep = {
        find_command = { "rg", "--hidden" },
      },
    },
  },
}