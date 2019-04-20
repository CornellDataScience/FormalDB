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
    if (command == "upload"){
        document.getElementById("upload").submit();
    }
    else if (command == "clear"){
        document.getElementById("output").innerHTML = "";
    }
    else {
            var data = {cmd: command};
            var url = "http://" + window.location.hostname + ":" + window.location.port +  "/command";
            $.ajax({
                url: url,
                type: "POST",
                data: data,
                success: function (r) {console.log("Sent command " + command)},
                error : function (r) {console.log(r)}
            });
    }
    document.getElementById("cli").value = ""
}
