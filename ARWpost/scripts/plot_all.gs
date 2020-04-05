*'reinit'
*'open ofs.ctl'

print_gif = 0 

'set display color white'
'set grads off'
'clear'
'set mpdset hires'
'set gxout shaded'

'q file'
rec=sublin(result,6)
_endfieldnr=subwrd(rec,5) + 6

'q ctlinfo'
rec=sublin(result,11)
_t=subwrd(rec,2)


***** Loop through time staring from t
t = 0
while ( t < _t )
  t = t + 1
  'set t 't
  fieldnr = 7
  
***** PLOT ALL THE FIELDS    
  while ( fieldnr < _endfieldnr )
    'clear'
    'q file'
    rec=sublin(result,fieldnr)
    say rec
    field=subwrd(rec,1)
    _levs=subwrd(rec,2)
  
    current_lev = 1
    'set z 'current_lev
    if ( _levs = 1 ) 
      'clear'
      'd ' field
      'run cbar'
      'draw title ' rec 

      if (print_gif = 1) 
        'printim 'field'.gif gif'
      else
        pull dummy
      endif
    else
    while ( current_lev < _levs )
      'clear'
      'set z 'current_lev
      'd ' field
      'run cbar'
      'draw title ' rec ' - lev ' current_lev
      current_lev = current_lev + 1

      if (print_gif = 1) 
        'printim 'field'_'current_lev'.gif gif'
      else
        pull dummy
      endif
    endwhile
    endif

  
    fieldnr = fieldnr + 1
  endwhile

endwhile
