;;; find-file-in-repository.el --- Quickly find files in a git, mercurial or other repository

;; Copyright (C) 2012  Samuel Hoffstaetter

;; Author: Samuel Hoffstaetter <samuel@hoffstaetter.com>
;; Keywords: files, convenience, repository, project, source control
;; URL: https://github.com/hoffstaetter/find-file-in-repository

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This libaray provides a drop-in replacement for find-file (ie. the
;; "C-x f" command), that auto-completes all files in the current git,
;; mercurial, or other type of repository. When outside of a
;; repository, find-file-in-repository conveniently drops back to
;; using find-file, (or ido-find-file), which makes it suitable
;; replacement for the "C-x f" keybinding.
;;
;; It is similar to, but much faster and more robust than the
;; find-file-in-project package. It relies on git/mercurial/etc to
;; provide fast cached file name completions, which means that
;; repository features such as .gitignore/.hgignore/etc are fully
;; supported out of the box.
;;
;; This library currently has support for:
;;     git, mercurial, darcs, bazaar, monotone, svn
;;
;; Contributions for support of other repository types are welcome.
;; Please send a pull request to
;; https://github.com/hoffstaetter/find-file-in-repository and I will
;; be happy to include your modifications.

;;; Code:

(defun ffir-shell-command (command file-separator)
  "Executes 'command' in the directory given by
  'repository-root', and returns the result split into a list
  with the 'file-separator' character"
  (lambda (repository-root)
    (delete "" (split-string
                (shell-command-to-string
                 (format "cd %s; %s"
                         (shell-quote-argument repository-root) command))
                file-separator))))

(defun ffir-locate-dominating-file-top (start-directory filename)
  "Returns the furthest ancester directory of 'start-directory'
   that contains a file of name 'filename'"
  (when start-directory
    (let ((next-directory (locate-dominating-file start-directory filename)))
      (if next-directory
          (ffir-locate-dominating-file-top next-directory filename)
        start-directory))))

(defun ffir-directory-contains-which-file (file-list directory)
  "Checks which of the files in 'file-list' exists inside
  'directory'. The file-list is a list (filename . value) tuples.
  For the first filename that exists in the directory, the
  corresponding value is returned. If 'directory' contains none
  of the filenames, nil is returned."
  (when file-list
    (if (file-exists-p (car (car file-list)))
        (cdr (car file-list))
      (ffir-directory-contains-p (cdr file-list) directory))))

(defun ffir-when-ido (ido-value non-ido-value)
  "Returns ido-value when ido is enabled, otherwise returns non-ido-value."
  (if (and (bound-p 'ido-mode) ido-mode)
      ido-value
    non-ido-value))

(defvar ffir-avoid-HOME-repository
  't
  "When set to nil, find-file-in-repository will accept the
  user's $HOME directory as a valid repository when it
  contains a .git/.hg/_darcs/(...) file.")

(defvar ffir-repository-types
  '((".git"   . ,(repository-shell-command "git ls-files -z"       "\0"))
    (".hg"    . ,(repository-shell-command "hg locate -0"          "\0"))
    ("_darcs" . ,(repository-shell-command "darcs show files -0"   "\0"))
    (".bzr"   . ,(repository-shell-command "bzr ls --versioned -0" "\0"))
    ("_MTN"   . ,(repository-shell-commnad "mtn list known"        "\n"))
    ;; svn repos must be searched differently from others since
    ;; every svn sub-directory contains a .svn folder as well
    (".svn"   . ,(repository-shell-command "svn list"              "\n")))
  "List of supported repository types for find-file-in-repository.
  The first entry in each tuple is a file name determining the
  repository type. The second entry in the tuple is a function
  that takes as argument the repository root, and returns the
  list of file names tracked by the repository.")

(defun find-file-in-repository (start-directory)
  "find-file-in-repository will autocomplete all files in the
   current git, mercurial or other type of repository, using
   ido-find-file when available. When the current file is not
   located inside of any repository, falls back on a regular
   find-file operation."
  (interactive)
  (let ((repo-directory (locate-dominating-file
                         start-directory
                         (apply-partially ffir-directory-contains-which-file
                                          ffir-repository-types))))
    (if (and repo-directory
             (not (and ffir-avoid-HOME-repository
                       (equal (getenv "HOME") repo-directory))))
        (let ((file-list-function (ffir-directory-contains-which-file
                                   ffir-repository-types repo-directory))
              (file-list (file-list-function repo-directory))
              (completing-read (ffir-when-ido ido-completing-read
                                              completing-read))
              (file (completing-read
                     "Find file in repository: " (mapcar 'car file-list))))
          (find-file (cdr (assoc file file-list))))
      (let ((find-file (ffir-when-ido ido-find-file find-file)))
        (execute-command 'find-file)))))

(defalias 'ffip 'find-file-in-project)

(progn
  (put 'ffir-repository-types 'safe-local-variable 'listp)
  (put 'ffir-avoid-HOME-repository 'safe-local-variable 'booleanp))

(provide 'find-file-in-repository)
;;; find-file-in-repository.el ends here
