# hmusica

Va leyendo los eventos de una entrada MIDI (ej. un teclado electrónico) y muestra las pulsaciones de tecla, también lee una entrada MIDI (en principio un fichero) y escribe las notas (sólo como notas básicas, no se deducen grupos complejos: articulación, arpegiado, dinámicas, ...).

![Captura de ejemplo](/screenshot.png?raw=true "Captura de ejemplo")

Por ejemplo

    hmusica-exe --windowWidth 800 --windowHeight 200 --deviceName "Digital Keyboard MIDI 1" --middleCindex 24 --keysNumber 61

