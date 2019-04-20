
/* =================================================== */
/* ===== Section 1: Require all the dependencies ===== */
/* =================================================== */

const express = require('express');
const bodyParser = require('body-parser');
const multer = require('multer');
var upload = multer({ dest: './uploads/' });
const hbs = require('hbs');
const logger = require('morgan');
var router = express.Router();
var engine = require('consolidate');
var child_p = require('child_process');
var path = __dirname;

// Define port for app to listen on
const port =  process.env.PORT || 4160;

/* ==================================================== */
/* ===== Section 2: Configure express middlewares ===== */
/* ==================================================== */

const app =  express();
app.use(bodyParser());  // to use bodyParser (for text/number data transfer between clientg and server)
app.engine('html', engine.mustache);
app.set('view engine', 'html');
//app.use(express.static(__dirname + '/public'));  // making ./public as the static directory
app.set('views', __dirname);  // making ./views as the views directory
app.use(logger('dev'));  // Creating a logger (using morgan)
app.use(express.json());
app.use('/css', express.static("./css"))
app.use('/js', express.static("./js"))
app.use('/vendor', express.static("./vendor"))
app.use('/scss', express.static("./scss"))
app.use('/img', express.static("./img"))
app.use(express.urlencoded({ extended: false }));

var expressWs = require('express-ws')(app);

var _stdout = "";
var _csv_proc;
/* ==================================== */
/* ===== Section 3: Making Routes ===== */
/* ==================================== */

var run_load_csv = function (){
    console.log("running load_csv");
    _csv_proc = child_p.spawn('../load_csv.byte', ['./uploads/uploaded.csv']);  
    _csv_proc.stdout.on('readable', function (){
        let data;
        while (data = this.read()){
            console.log(data.toString());
            _stdout = _stdout + data.toString();
        }
    });
    _csv_proc.stdin.on
}

router.use(function (req,res,next) {
    console.log("/" + req.method);
    next();
  });

router.get("/",function(req,res){
    res.sendFile(path + "/index.html");
    });  

router.get("/client.js", function(req, res){
    res.sendFile(path + "/client.js")
});

router.ws('/echo', function(ws, req){
    ws.on('message', function(msg) {
        ws.send("received a message")
    });
});

router.ws('/stdout', function(ws, req){
    ws.on('message', function(msg) {
        if (msg == "request stdout"){
            if (_stdout != ""){
                ws.send(_stdout);
                _stdout = "";
            }
        }
    });
});

var storage = multer.diskStorage({
    destination: function (req, file, cb) {
        cb(null, './uploads')
    },
    filename: function (req, file, cb) {
        cb(null, "uploaded.csv")
    }
});



upload = multer({ storage: storage })

app.post('/command', (req, res) => {
    console.log("Processing command :" + req.body.cmd);
    rec_cmd = req.body.cmd;
    //default behavior
    if (rec_cmd == "help"){
        _stdout = "Current commands: upload, load csv";
    }
    else if (rec_cmd == "upload") {
        _stdout = "FAILURE upload command should be processed client-side."
    }
    else if (rec_cmd == "load csv"){
        __stdout = "loading csv";
        run_load_csv();
    }
    else if (rec_cmd.split(' ')[0] == "select"){
        console.log("sending command " + rec_cmd + "to process")
        _csv_proc.stdin.write(rec_cmd + "\n")
    }
    else if (rec_cmd == "print db"){
        _csv_proc.stdin.write("print\n")
    }
    else if (rec_cmd == "kill db"){
        _stdout += "Exiting FormalDB process"
        _csv_proc.stdin.write("exit")
        _csv_proc.stdin.end()
    }
    else if (rec_cmd == "force kill db proc"){
        _stdout += "Sending kill signal to FormalDB process, this is a hard exit."
        _csv_proc.kill();
    }
    else{
        _stdout = rec_cmd + " is not a recognized command"
    }
    res.status(204);
    res.end();
});

app.post('/upload', upload.single('csv_file'), (req, res) => {
    if (req.file) {
        console.log('Uploading file...');
        var filename = req.file.filename;

        var uploadStatus = 'File Uploaded Successfully';
        console.log("File " + filename + " loaded.")

        _stdout = "File uploaded successfully"
        

    } else {
        console.log('No File Uploaded');
        var filename = 'FILE NOT UPLOADED';
        var uploadStatus = 'File Upload Failed';
        _stdout = "File upload FAILED"
    }
    
    res.status(204)
    res.end()
    
});




// GET / route for serving index.html file
app.use("/", router);


// To make the server live
app.listen(port, () => {
    console.log(`CDS Formal DB App is live on port ${port}`);
});
