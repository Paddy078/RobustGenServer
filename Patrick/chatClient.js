window.onload = function() {
    var chatMessagesContainerId = "chatMessages";

    var xmlhttp = new XMLHttpRequest(); //ajax request object

    var getNewMessagesUrlPart = "/msg";

    var serverUrl = "http://localhost:8080"; //window.location.href.split('?')[0];
    var serverUrlToGetNewMessages = serverUrl + getNewMessagesUrlPart;
    var serverUrlToPostNewMessage = serverUrl + "";

    var isGetNewMessagesRequestResponse = function() {
        return xmlhttp.responseURL.indexOf(getNewMessagesUrlPart) != -1;
    }

    xmlhttp.onreadystatechange = function () {
        if (xmlhttp.readyState == XMLHttpRequest.DONE) {
            if (xmlhttp.status == 200) {
                if(isGetNewMessagesRequestResponse()){
                    var responseJson = JSON.parse(xmlhttp.responseText);
                    appendNewChatMessagesOutput(responseJson);
                }
            }
        }
    }

    var appendNewChatMessagesOutput = function(messagesJson){
        var messagesTable = document.getElementById(chatMessagesContainerId);
        messagesTable.innerHTML = "";
        for(i = 0; i < messagesJson.length; i++){
            messagesTable.appendChild(createMessageLineTableRecord(messagesJson[i].from, messagesJson[i].message));
        }
    }

    var createMessageLineTableRecord = function(from, message) {
        var messageLineTableRecord = document.createElement("TR");
        var fromTableDataElement = createTableData(from);
        fromTableDataElement.style.borderRight = "2px solid";
        messageLineTableRecord.appendChild(fromTableDataElement);

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
        if(document.getElementById("messageInputField").value != "") {
            //to implement
            xmlhttp.open("POST", serverUrlToPostNewMessage, true);
            xmlhttp.send(null);
        }
    }

    var sendGetNewMessagesRequest = function () {
        xmlhttp.open("GET", serverUrlToGetNewMessages, true);
        xmlhttp.send(null);
    }


    document.getElementById("sendMessageButton").onclick = postMessageToServer;


    sendGetNewMessagesRequest();
    //setInterval(sendGetNewMessagesRequest, 3000);
}






