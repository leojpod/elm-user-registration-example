import './style.css'
import { Elm } from './elm/Main.elm'
import '@fortawesome/fontawesome-free/css/all.min.css'

const app = Elm.Main.init({ flags: {} })

app.ports.exportForm.subscribe(console.log)
