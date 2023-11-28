import { useState } from 'react';
import Dropdown from 'react-dropdown';
import 'react-dropdown/style.css';
import './App.css';

export default function Uploads() {
    const [province, setProvince] = useState(null);
    const [t1input, setT1input] = useState(null);
    const [submitted, setSubmitted] = useState(null);
    const [t1output, setT1output] = useState(null);
    const [error, setError] = useState("");
    
    function handleUpload (event) {
        setT1input(event.target.files[0]);
        setSubmitted(false);
        setError("");
        setT1output(null);
    }

    function handleResponse (response) {
        if (response.ok) {
            response.blob().then(setT1output);
        }
        else {
            response.text().then(setError);
        }
    }

    function handleSubmit (event) {
        if (t1input && province) {
            fetch("/t1/PDF/" + province.value.code, {"method": "POST", "mode": "same-origin", "body": t1input})
                .then(handleResponse, setError)
                .catch(setError);
            setSubmitted(true);
        }
    }

    const provinces = [
        {label: 'Alberta',                   value: {code: 'AB', formPrefix: '5015'}},
        {label: 'British Columbia',          value: {code: 'BC', formPrefix: '5010'}},
        {label: 'Manitoba',                  value: {code: 'MB', formPrefix: '5015'}},
        {label: 'New Brunswick',             value: {code: 'NB', formPrefix: '5000'}},
        {label: 'Newfoundland and Labrador', value: {code: 'NL', formPrefix: '5001'}},
        {label: 'Northwest Territories',     value: {code: 'NT', formPrefix: '5012'}},
        {label: 'Nova Scotia',               value: {code: 'NS', formPrefix: '5015'}},
        {label: 'Nunavut',                   value: {code: 'NU', formPrefix: '5014'}},
        {label: 'Ontario',                   value: {code: 'ON', formPrefix: '5006'}},
        {label: 'PEI',                       value: {code: 'NB', formPrefix: '5000'}},
        {label: 'Quebec',                    value: {code: 'QC', formPrefix: '5005'}},
        {label: 'Saskatchewan',              value: {code: 'SK', formPrefix: '5015'}},
        {label: 'Yukon',                     value: {code: 'YT', formPrefix: '5011'}}
    ];

    return (
      <>
        <h2>T1 form completion</h2>

        <h3>Step 1. <Dropdown className='provinceRoot' menuClassName='provinceMenu' options={provinces} default={province} onChange={setProvince} placeholder="Select your province"/></h3>

        {province && <>
         <h3>Step 2. Download the <em>fillable</em> PDF form <tt>{province.value.formPrefix}-r-fill-22e.pdf</tt> from <a href="https://canada.ca">canada.ca</a></h3>
         <h3>Step 3. Fill in the downloaded T1 form.</h3>
         <p>Don't bother with any fields that are calculated from other fields in the same form, that part will be done for you automatically.</p>
         <p>You can leave out your name, SIN, and other private data, since they're not affecting any numbers.</p>
         <h3>Step 4. Upload the filled form: <input type="file" name="T1 PDF" onChange={handleUpload}/></h3>
         <h3>Step 5. <button name="Calculate" disabled={submitted !== false} onClick={handleSubmit}>Calculate</button></h3>
         {t1output && <>
          <h3>Step 6. <a name="Download" download="t1.pdf"
                         href={URL.createObjectURL(new File([t1output],
                                                            {name: "t1.pdf", type: 'application/PDF'}))}>Download</a>
          </h3>
          <h3>Step 7. Carefully examine the downloaded form. You can also make adjustments and go to Step 4 again.</h3>
          <h3>Step 8. Fill in your name, address, SIN, and other private information.</h3>
          <h3>Step 9. Print the form and mail your return to CRA.</h3>
          </>
         }
         </>
        }
        <p>{error.toString()}</p>
      </>
    );
}
