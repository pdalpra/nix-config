{ config, pkgs, ... }:

let
  theme = "catppuccin_${config.catppuccin.flavor}";
in
{
  home.sessionVariables = {
    EDITOR = "vim";
  };

  programs.vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      vim-nix
      vim-scala
      catppuccin-vim
    ];

    extraConfig = ''
      "General configuration

      set nocompatible " Break compatibilty with Vi
      filetype off
      filetype plugin indent on
      set bs=2 " Backspace can delete almost everything
      set number " Numbers lines
      set numberwidth=4 " Up to 9999 lines
      set cursorline " Highlight current line
      set cursorcolumn " Hightlight current column
      set nobackup " No backup
      set noswapfile " No swap file
      set clipboard+=unnamed " Enable system wide clipboard
      set hidden " Switch beetwen buffers without saving
      set autoread " Re read file if changed from the outside
      set ruler
      set scrolloff=20
      set shortmess=aOstT " No press ENTER to continue
      filetype plugin on " Enable plugins

      " Indentation

      set autoindent " Auto identation
      set smartindent " Guess next indentation
      set expandtab " Tabs = spaces
      set shiftround " Round shifting

      " Looks and syntax highlighting

      syntax enable " Enable
      set t_Co=256 " Terminal has 256 colors
      set background=dark
      set wildmenu " Enables wild Menu
      set wildignore=*.o,*.cmo,*.cmi,*.pyc " Ignonre thoses extensions in wild menu
      set wildmode=list:longest " Show all possibilities in wild menu
      colorscheme ${theme} " Colorscheme
      set novisualbell " Don't blink, don't even blink, blink and you're dead !
      set showcmd " Show command being typed
      set showmatch " Show matching brackets
      set ttyfast
    '';
  };
}
