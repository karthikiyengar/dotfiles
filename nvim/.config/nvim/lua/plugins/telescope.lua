return {
  "nvim-telescope/telescope.nvim",
  opts = {
    keys = {
      { "<leader>,", false },
    },
    defaults = {
      vimgrep_arguments = {
        "rg",
        "--hidden",
        "--color=never",
        "--no-heading",
        "--with-filename",
        "--line-number",
        "--column",
        "--smart-case",
      },
    },
    pickers = {
      find_files = {
        hidden = true,
      },
      live_grep = {
        find_command = { "rg", "--hidden" },
      },
    },
  },
}