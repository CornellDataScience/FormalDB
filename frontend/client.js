window.addEventListener('load', function() {
    document.getElementById('cli').onkeydown = function(e){
        if(e.keyCode == 13){
            e.preventDefault();
            parse_command(document.getElementById("cli").value)

        }
     };

     var url_e = "ws://" + window.location.hostname + ":" + window.location.port +  "/echo"
     var url_s = "ws://" + window.location.hostname + ":" + window.location.port +  "/stdout"
     const connection = new WebSocket(url_e)
     const connection_s = new WebSocket(url_s)

     connection_s.onopen = () => {
         window.setInterval(check_stdout, 1000);
         function check_stdout() {
            connection_s.send("request stdout")
         }
     }

     connection_s.onmessage = e => {
         var old_content = document.getElementById("output").innerHTML;
         document.getElementById("output").innerHTML = old_content + "<br>" + e.data
     }

     connection.onopen = () => {
         connection.send("hello")
     }

     connection.onmessage = e => {
        console.log(e.data)
     }


})



function parse_command (command) {
    switch(command){
        case "upload": document.getElementById("upload").submit();
            break;
        
        default: 
            $.post((window.location.hostname + ":" + window.location.port +  "/command"), command, function (data, status) {
                console.log("Sent command " + command)
            });
        break;
    }
    document.getElementById("cli").value = ""
}