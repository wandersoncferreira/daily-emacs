
(defvar prot/elfeed-bongo-playlist "*Bongo-Elfeed Queue*"
  "Name of the Elfeed+Bongo multimedia playlist.")

(defun prot/elfeed-bongo-insert-item ()
  "Insert `elfeed' multimedia links in `bongo' playlist buffer.
The playlist buffer has a unique name so that it will never
interfere with the default `bongo-playlist-buffer'."
  (interactive)
  (with-eval-after-load 'bongo
    (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                      elfeed-show-entry
                    (elfeed-search-selected :ignore-region)))
           (link (elfeed-entry-link entry))
           (enclosure (elt (car (elfeed-entry-enclosures entry)) 0))
           (url (or enclosure link))
           (title (elfeed-entry-title entry))
           (bongo-pl prot/elfeed-bongo-playlist)
           (buffer (get-buffer-create bongo-pl)))
      (unless (bongo-playlist-buffer)
        (bongo-playlist-buffer))
      (display-buffer buffer)
      (with-current-buffer buffer
        (when (not (bongo-playlist-buffer-p))
          (bongo-playlist-mode)
          (setq-local bongo-library-buffer (get-buffer "*elfeed-search*"))
          (setq-local bongo-enabled-backends '(mpv))
          (bongo-progressive-playback-mode))
        (goto-char (point-max))
        (bongo-insert-uri url title)
        (bongo-insert-comment-text (format "     ==> %s\n" url))
        (let ((inhibit-read-only t))
          (delete-duplicate-lines (point-min) (point-max)))
        (bongo-recenter))
      (message "Enqueued %s “%s” in %s"
               (if enclosure "podcast" "video")
               (propertize title 'face 'italic)
               (propertize bongo-pl 'face 'bold)))))

(define-bongo-backend mpv
  ;; :constructor 'bongo-start-mpv-player
  :program-name 'mpv
  :extra-program-arguments nil
  :matcher '((local-file "file:" "http:" "ftp:")
             "ogg" "flac" "mp3" "mka" "wav" "wma"
             "mpg" "mpeg" "vob" "avi" "ogm" "mp4" "mkv"
             "mov" "asf" "wmv" "rm" "rmvb" "ts")
  :matcher '(("mms:" "mmst:" "rtp:" "rtsp:" "udp:" "unsv:"
              "dvd:" "vcd:" "tv:" "dvb:" "mf:" "cdda:" "cddb:"
              "cue:" "sdp:" "mpst:" "tivo:") . t)
  :matcher '(("http:" "https:") . t))

(when (bk/add-load-path "apps/emms" "bongo")
  (bk-auto-loads "bongo" #'bongo)
  (eval-after-load 'bongo
    '(setq bongo-prefer-library-buffers nil
           bongo-join-inserted-tracks nil
           bongo-mark-played-tracks t
           bongo-enabled-backends '(vlc mpv))))
