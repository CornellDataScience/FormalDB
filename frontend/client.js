window.addEventListener('load', function() {
    document.getElementById('cli').onkeydown = function(e){
        if(e.keyCode == 13){
            e.preventDefault();
             parse_command(document.getElementById("cli").value)
        }
     };


})



function parse_command (command) {
    if (command == "upload"){
        document.getElementById("upload").submit();
    }
}