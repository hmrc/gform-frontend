/**
 * Created by harrison on 13/01/17.
 */

var counter = 1;
var limit = 22;
function addInput(divName){
    if (counter == limit)  {
        alert("You have reached the limit of adding " + counter + " inputs");
    }
    else {
        var newdiv = document.createElement('div' + counter);
        newdiv.innerHTML ='<div style="float: left;" id="name_'+ counter +'"> <dl class=" " id="environmentalBody1_'+ counter +'_bodyName_field"><dl class=" " id="environmentalBody1_'+ counter +'_bodyName_field"> <dt><label for="environmentalBody1_'+ counter +'_bodyName">Name</label></dt> <dd> <input type="text" id="environmentalBody1_'+ counter +'_bodyName" name="environmentalBody1['+ counter +'].bodyName" value="" inputdivclass="col-sm-8" placeholder="" class="form-field-group"> </dd> </dl></div> ' +
            '              <div style="float: right;" id="amount_'+ counter +'"> <dl class=" " id="environmentalBody1_'+ counter +'_amount_field"> <dt><label for="environmentalBody1_'+ counter +'_amount">Amount</label></dt> <dd> <input type="text" id="environmentalBody1_'+ counter +'_amount" name="environmentalBody1['+ counter +'].amount" value="" inputdivclass="col-sm-8" placeholder="" class="form-field-group"> </dd> </dl> </div>';
        document.getElementById("newField").appendChild(newdiv);
        counter++;
    }
}

function removeField(divName) {
    if (counter == 1){
        alert("One field is mandatory")
    }
    else {
        counter--;
        var nameElement = document.getElementById('name_' + counter + ''); // notice the change
        nameElement.parentNode.removeChild(nameElement);
        var amountElement = document.getElementById('amount_' + counter + ''); // notice the change
        amountElement.parentNode.removeChild(amountElement);
    }


}