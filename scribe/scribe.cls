\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{scribe}[2019/09/03 Scribe Class]
\LoadClass[12pt]{amsart}

% Package requirements.
\RequirePackage{npjg}     % my global defaults
\RequirePackage{mathrsfs} % formal script
\RequirePackage{calrsfs}
\RequirePackage{enumerate}
\RequirePackage{tikz}
\RequirePackage{float}
\RequirePackage{mdframed}

% Page layout.
\oddsidemargin 0in \evensidemargin 0in \marginparwidth 40pt
\marginparsep 10pt \topmargin 0pt \headsep 0in \headheight 0in
\textheight 8.5in \textwidth 6.5in \brokenpenalty=10000

\DeclareMathAlphabet{\pazocal}{OMS}{zplm}{m}{n}

% Property definitions.
% make title optional; don't show it if it is.
\def\ifempty#1{\def\temp{#1}\ifx\temp\empty}
\renewcommand{\title}[1]{\ifempty{#1}\else\def\@title{: #1}\fi}
\renewcommand{\author}[1]{\def\@author{#1}}
\newcommand{\course}[1]{\def\@course{#1}}
\newcommand{\term}[1]{\def\@term{#1}}
\newcommand{\type}[1]{\def\@type{#1}}
\newcommand{\period}[1]{\def\@period{#1}}
\newcommand{\lector}[1]{\def\@lector{#1}}

% Scribe title.
% if the title is empty, don't throw an error
% (lectures aren't required to have titles)
\newdimen\headerwidth
\makeatletter
\renewcommand{\@maketitle}{%
  \noindent
  \begin{center}
    \framebox{
      \vbox{
        \headerwidth=\textwidth
        \advance\headerwidth by -0.22in
        \hbox to \headerwidth {{\bf \@course \hfill \@term} }
        \vspace{4mm}
        \hbox to \headerwidth {{\Large \hfill \@type \space \@period \@title \hfill}}
        \vspace{2mm}
        \hbox to \headerwidth {\hfill \@date \hfill}
        \vspace{2mm}
        \hbox to \headerwidth {{\it \@lector \hfill (\@author)}}
      }
    }
  \end{center}
  \vspace*{4mm}
}
\makeatother

% Setting the theorem style back to plain in case theorems are defined in the main file
\theoremstyle{plain}

% Frame initialization.
\mdfsetup{
  skipabove=.25\baselineskip,
  skipbelow=.5\baselineskip,
  innerbottommargin=.5\baselineskip
}
% Theorem environments from npjg.
\surroundwithmdframed{exercise}
\surroundwithmdframed{definition}
\surroundwithmdframed{theorem}
\surroundwithmdframed{corollary}
\surroundwithmdframed{lemma}
\surroundwithmdframed{proposition}
\surroundwithmdframed{remark}
\surroundwithmdframed{proof}
