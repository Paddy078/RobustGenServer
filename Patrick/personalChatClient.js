window.onload = function() {
    var chatMessagesContainerId = "chatMessages";

    var xmlhttp = new XMLHttpRequest();

	var serverUrl = "http://192.168.2.107:8080";

    var fromSelector = document.getElementById("fromSelector");
    var toSelector = document.getElementById("toSelector");

    var isGetNewMessagesRequestResponse = function() {
        return xmlhttp.responseText != "[]" && xmlhttp.responseText != "";
    }

    var getSelectedFromValue = function(){
        return fromSelector.options[fromSelector.selectedIndex].value;
    }

    var getSelectedToValue = function(){
        return toSelector.options[toSelector.selectedIndex].value;
    }

    xmlhttp.onreadystatechange = function () {
        if (xmlhttp.readyState == XMLHttpRequest.DONE) {
            if (xmlhttp.status == 200) {
                if(isGetNewPersonalMessagesRequest()){
                    var messagesJson = JSON.parse(cleanJson(xmlhttp.responseText));
                    appendNewChatMessagesOutput(messagesJson);
                }
                else if(isGetRegisteredUsersRequest()){
                    var usersJson = JSON.parse(cleanJson(xmlhttp.responseText));
                    initializeFromAndToSelector(usersJson);
                }
            }
        }
    }

    //temp until json erlang bug is fixed
    var cleanJson = function(jsonToClean) {
        return jsonToClean.replace(/\"\"/g, '\"');
    }

    var appendNewChatMessagesOutput = function(messagesJson){
        var messagesTable = document.getElementById(chatMessagesContainerId);
        messagesTable.innerHTML = "";
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

    var isGetRegisteredUsersRequest = function(){
        return xmlhttp.responseURL.indexOf("/usr") != -1;
    }

    var isGetNewPersonalMessagesRequest = function(){
        return xmlhttp.responseURL.indexOf("/personalMsg/") != -1;
    }

    var sendGetRegisteredUsersRequest = function(){
        var url = serverUrl + "/usr";
        xmlhttp.open("GET", url, true);
        xmlhttp.send(null);
    }

    var sendPersonalMessagePostRequest = function () {
        var messageInputField = document.getElementById("messageInputField");
        var messageInputFieldValue = messageInputField.value;
        if(messageInputFieldValue != "") {
            var url = serverUrl + "/personalMsg";
            xmlhttp.open("POST", url, true);
            var fromValue = getSelectedFromValue();
            var toValue = getSelectedToValue();
            var messageValue = messageInputField.value;
            var jsonString = JSON.stringify({from:fromValue, to:toValue, message:messageValue});
            xmlhttp.send(jsonString);
            messageInputField.value = "";
        }
    }

    var sendGetNewPersonalMessagesRequest = function(){
        // example-url: /personalMsg/Patrick/Timo/4
        //with personal messages always get all messages between those 2 chat partner(=> "/0"). to work with an index would need a lot more js code
        var url = serverUrl + "/personalMsg/" + getSelectedFromValue() + "/" + getSelectedToValue() + "/0";
        xmlhttp.open("GET", url, true);
        xmlhttp.send(null);
    }

    var triggerFromAndToSelectorInitialization = function(){
        sendGetRegisteredUsersRequest();
    }

    var initializeFromAndToSelector = function(usersJson) {
        for(i = 0; i < usersJson.length; i++){
            var fromOption = document.createElement("option");
            fromOption.text = usersJson[i].name;
            fromSelector.add(fromOption);
            var toOption = document.createElement("option");
            toOption.text = usersJson[i].name;
            toSelector.add(toOption);
        }
    }

    document.getElementById("sendMessageButton").onclick = sendPersonalMessagePostRequest;
    fromSelector.onchange = sendGetNewPersonalMessagesRequest;
    toSelector.onchange = sendGetNewPersonalMessagesRequest;
    triggerFromAndToSelectorInitialization();
    setInterval(sendGetNewPersonalMessagesRequest, 1000);
}







