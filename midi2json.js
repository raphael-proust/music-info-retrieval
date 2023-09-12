const fs = require('fs')
const { Midi } = require('@tonejs/midi')

const midiData = fs.readFileSync(process.argv[2])
const midi = new Midi(midiData)
fs.writeFileSync("/dev/stdout", JSON.stringify(midi))
