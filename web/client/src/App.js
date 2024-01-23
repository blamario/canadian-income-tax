import { useState } from 'react';
import Dropdown from 'react-dropdown';
import 'react-dropdown/style.css';
import './App.css';

const provinces = [
    {label: 'Alberta',                   value: {code: 'AB', prefixT1: '5015', prefix428: '5009'}},
    {label: 'British Columbia',          value: {code: 'BC', prefixT1: '5010', prefix428: '5010', has479: true}},
    {label: 'Manitoba',                  value: {code: 'MB', prefixT1: '5015', prefix428: '5007'}},
    {label: 'New Brunswick',             value: {code: 'NB', prefixT1: '5000'}},
    {label: 'Newfoundland and Labrador', value: {code: 'NL', prefixT1: '5001'}},
    {label: 'Northwest Territories',     value: {code: 'NT', prefixT1: '5012'}},
    {label: 'Nova Scotia',               value: {code: 'NS', prefixT1: '5015'}},
    {label: 'Nunavut',                   value: {code: 'NU', prefixT1: '5014'}},
    {label: 'Ontario',                   value: {code: 'ON', prefixT1: '5006', prefix428: '5006', has479: true}},
    {label: 'PEI',                       value: {code: 'NB', prefixT1: '5000'}},
    {label: 'Quebec',                    value: {code: 'QC', prefixT1: '5005'}},
    {label: 'Saskatchewan',              value: {code: 'SK', prefixT1: '5015'}},
    {label: 'Yukon',                     value: {code: 'YT', prefixT1: '5011'}}
];

export default function Uploads() {
    const [province, setProvince] = useState(null);
    const [inputs, setInputs] = useState({});
    const [submitted, setSubmitted] = useState(null);
    const [output, setOutput] = useState(null);
    const [error, setError] = useState("");

    function handleUpload (formKey) {
        return (event) => {
            let newInputs = Object.assign(inputs);
            newInputs[formKey] = event.target.files[0];
            setInputs(newInputs);
            setSubmitted(false);
            setError("");
            setOutput(null);
        }
    }

    function handleResponse (response) {
        if (response.ok) {
            response.blob().then(setOutput);
        }
        else {
            response.text().then(setError);
        }
    }

    function handleSubmit (event) {
        if (inputs && province) {
            const forms = new FormData();

            for (const key in inputs)
                forms.append(key, inputs[key]);
            fetch("/t1/PDF/" + province.value.code, {method: "POST", mode: "same-origin", body: forms})
                .then(handleResponse, setError)
                .catch(setError);
            setSubmitted(true);
        }
    }

    const forms = province?.value.prefix428 ? "forms" : "T1 form";
    const formsAgain = province?.value.prefix428 ? "forms" : "form";
    const download = province?.value.prefix428
          ? {name: "tax-forms.zip", type: 'application/zip'}
          : {name: "T1.pdf", type: 'application/PDF'};
    
    return (
      <>
        <h2>T1 form completion</h2>

        <h3>Step 1. <Dropdown className='provinceRoot' menuClassName='provinceMenu' options={provinces} default={province} onChange={setProvince} placeholder="Select your province"/></h3>

        {province && <>
         <h3>Step 2. Download the <em>fillable</em> PDF {province.value.prefix428 ? <>T1 and 428 forms, <tt>{province.value.prefixT1}-r-fill-22e.pdf</tt> and <tt>{province.value.prefix428}-c-fill-22e.pdf</tt>,</> : <>form <tt>{province.value.prefixT1}-r-fill-22e.pdf</tt></>} from <a href="https://canada.ca">canada.ca</a></h3>
         <h3>Step 3. Fill in the downloaded {forms}</h3>
         <p>Don't bother with any fields that are calculated from other fields in the same {formsAgain}, that part will be done for you automatically.</p>
         <p>You can leave out your name, SIN, and other private data, since they're not affecting any numbers.</p>
         <h3>Step 4. Upload the filled {formsAgain}:</h3>
         <dl>
         <dt>T1</dt>
         <dd><input type="file" name="T1 PDF" onChange={handleUpload("T1")}/></dd>
         {province.value.prefix428
          ? <>
          <dt>{province.value.code}428</dt>
          <dd><input type="file" name="428 PDF" onChange={handleUpload("428")}/></dd>
          </>
          : ""}
         </dl>
         <h4>You can also optionally upload the following forms, if they apply:</h4>
         <dl>
         {province.value.has479
          ? <>
          <dt>{province.value.code}479 tax credits form:</dt>
          <dd><input type="file" name="479 PDF" onChange={handleUpload("479")}/></dd>
          </>
          : ""}
         <dt>Schedule 9</dt>
         <dd><input type="file" name="Schedule 9 PDF" onChange={handleUpload("Schedule9")}/></dd>
         <dt>Schedule 11</dt>
         <dd><input type="file" name="Schedule 11 PDF" onChange={handleUpload("Schedule11")}/></dd>
         </dl>
         <h3>Step 5. <button name="Calculate" disabled={submitted !== false} onClick={handleSubmit}>Calculate</button></h3>
         {output && <>
          <h3>Step 6. <a name="Download" download={download.name}
                       href={URL.createObjectURL(new File([output], download))}>Download</a>
          </h3>
          <h3>Step 7. Carefully examine the downloaded {forms}. You can also make adjustments and go to Step 4 again.</h3>
          <h3>Step 8. Fill in your name, address, SIN, and other private information.</h3>
          <h3>Step 9. Print the {formsAgain} and mail your return to CRA.</h3>
          </>
         }
         </>
        }
        <p>{error.toString()}</p>
      </>
    );
}
