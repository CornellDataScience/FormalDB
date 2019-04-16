
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
var path = __dirname;

// Define port for app to listen on
const port =  process.env.PORT || 3110;

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


/* ==================================== */
/* ===== Section 3: Making Routes ===== */
/* ==================================== */

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

var storage = multer.diskStorage({
    destination: function (req, file, cb) {
        cb(null, './uploads')
    },
    filename: function (req, file, cb) {
        cb(null, "uploaded.csv")
    }
});

upload = multer({ storage: storage })

app.post('/upload', upload.single('csv_file'), (req, res) => {
    if (req.file) {
        console.log('Uploading file...');
        var filename = req.file.filename;


        var uploadStatus = 'File Uploaded Successfully';
        console.log("File " + filename + " loaded.")
    } else {
        console.log('No File Uploaded');
        var filename = 'FILE NOT UPLOADED';
        var uploadStatus = 'File Upload Failed';
    }
    
    /* ===== Add the function to save filename to database ===== */
    
    //res.render("index.html")
});




// GET / route for serving index.html file
app.use("/", router);


// To make the server live
app.listen(port, () => {
    console.log(`CDS Formal DB App is live on port ${port}`);
});
