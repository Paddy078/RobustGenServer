window.onload = function() {
    var chatMessagesContainerId = "chatMessages";

    var xmlhttp = new XMLHttpRequest(); //ajax request object

    //var serverUrl = "http://localhost:8080/msg";
	var serverUrl = "http://192.168.2.107:8080/msg";

    var lastMessageIndex = 0;

    var isGetNewMessagesRequestResponse = function() {
        return xmlhttp.responseText != "[]" && xmlhttp.responseText != "";

    }

    xmlhttp.onreadystatechange = function () {
        if (xmlhttp.readyState == XMLHttpRequest.DONE) {
            if (xmlhttp.status == 200) {
                if (isGetNewMessagesRequestResponse()) {
                    //temp until json erlang bug is fixed
                    var tempJson = xmlhttp.responseText.replace(/\"\"/g, '\"');
                    var responseJson = JSON.parse(tempJson);

                    //var responseJson = JSON.parse(xmlhttp.responseText);

                    appendNewChatMessagesOutput(responseJson);
                }
            }
        }
    }

    var appendNewChatMessagesOutput = function(messagesJson){
        var messagesTable = document.getElementById(chatMessagesContainerId);
        //messagesTable.innerHTML = "";
        for(i = 0; i < messagesJson.length; i++){
            messagesTable.appendChild(createMessageLineTableRecord(messagesJson[i].time, messagesJson[i].from, messagesJson[i].message));
            lastMessageIndex = messagesJson[i].id;
        }
    }

    var createMessageLineTableRecord = function(time, from, message) {
        var messageLineTableRecord = document.createElement("TR");

        var fromTableDataElement = createTableData(from );
        messageLineTableRecord.appendChild(fromTableDataElement);

        var timeTableDataElement = createTableData("(" + time + ") :");
        messageLineTableRecord.appendChild(timeTableDataElement);

        messageLineTableRecord.appendChild(createTableData(message));
        return messageLineTableRecord;
    }

    var createTableData = function(data) {
        var element = document.createElement("TD");
        element.innerHTML = data;
        element.style.padding = "5px";
        return element;
    }

    var postMessageToServer = function() {
        var messageInputField = document.getElementById("messageInputField");
        var messageInputFieldValue = messageInputField.value;
        var nameInputFieldValue = document.getElementById("nameInputField").value;
        if(nameInputFieldValue == ""){
            alert("enter a name first to send messages");
        }
        else{
            if(messageInputFieldValue != "") {
                xmlhttp.open("POST", serverUrl, true);
                xmlhttp.send(JSON.stringify({from:nameInputFieldValue, message:messageInputFieldValue}));
                messageInputField.value = "";
            }
        }
    }

    var sendGetNewMessagesRequest = function () {
        var urlWithLastMessageIndex = serverUrl + "/" + lastMessageIndex;

        xmlhttp.open("GET", urlWithLastMessageIndex, true);
        xmlhttp.send(null);
    }


    document.getElementById("sendMessageButton").onclick = postMessageToServer;


    sendGetNewMessagesRequest();
    setInterval(sendGetNewMessagesRequest, 1000);
}




//fromTableDataElement.style.borderRight = "2px solid";



