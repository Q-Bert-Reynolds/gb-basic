;KBHandleCharacter
;KBHandleArrowKey
;KBHandleTab
;KBHandleEnter
;KBHandleBackspace
;KBHandleDelete
;KBHandleFunctionKey
;KBHandleEscape

KBHandleCharacter::;a = character in ASCII
  jp charoutns

KBHandleArrowKey::;a = DPad key
  
  ret

KBHandleTab::

  ret

KBHandleEnter::
  ld a, cr
  jp charoutns

KBHandleBackspace::
  ld a, BS
  jp charoutns

KBHandleDelete::
  
  ret

KBHandleFunctionKey::;a = function key

  ret

KBHandleEscape::
  
  ret