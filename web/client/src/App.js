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
        if (t1input) {
            fetch("/t1/PDF", {"method": "POST", "mode": "same-origin", "body": t1input})
                .then(handleResponse, setError)
                .catch(setError);
            setSubmitted(true);
        }
    }

    const provinces = [
        {label: 'Alberta',          value: {code: 'AB', formPrefix: '5015'}},
        {label: 'British Columbia', value: {code: 'BC', formPrefix: '5010'}},
        {label: 'Ontario',          value: {code: 'ON', formPrefix: '5006'}},
        {label: 'Quebec',           value: {code: 'QC', formPrefix: '5005'}}]

    return (
      <>
        <h2>T1 form completion</h2>

        <h3>Step 1. <Dropdown className='provinceRoot' options={provinces} default={province} onChange={setProvince} placeholder="Select your province"/></h3>

        {province && <>
         <h3>Step 2. Download the <em>fillable</em> PDF form <tt>{province.value.formPrefix}-r-fill-22e.pdf</tt> from <a href="https://canada.ca">canada.ca</a></h3>
         <h3>Step 3. Fill in the downloaded T1 form; don't bother with any fields that are calculated from other fields in the same form, that part will be performed automatically.</h3>
         <h3>Step 4. Upload the filled form: <input type="file" name="T1 PDF" onChange={handleUpload}/></h3>
         <h3>Step 5. <button name="Calculate" disabled={submitted !== false} onClick={handleSubmit}>Calculate</button></h3>
         {t1output &&
          <h3>Step 6. <a name="Download" download="t1.pdf"
                         href={URL.createObjectURL(new File([t1output],
                                                            {name: "t1.pdf", type: 'application/PDF'}))}>Download</a>
          </h3>
         }
         </>
        }
        <p>{error.toString()}</p>
      </>
    );
}
