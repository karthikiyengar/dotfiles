-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

-- Use system clipboard (disabled in SSH to let OSC 52 work)
vim.opt.clipboard = vim.env.SSH_CONNECTION and "" or "unnamedplus"
vim.g.suda_smart_edit = 1
vim.g.blamer_enabled = true
vim.opt.title = true
vim.opt.relativenumber = false
