import { useRef, useState } from 'react';
import Dropdown from 'react-dropdown';
import ReactModal from 'react-modal';
import 'react-dropdown/style.css';
import 'react-modal-overlay/dist/index.css'
import '../../static/shared.css';
import '../../static/t4.css';
import T4 from './t4';
import './App.css';

const provinces = [
    {label: 'Alberta',                   value: {code: 'AB', href: 'alberta',
                                                 prefixT1: '5015', prefix428: '5009'}},
    {label: 'British Columbia',          value: {code: 'BC', href: 'british-columbia',
                                                 prefixT1: '5010', prefix428: '5010', has479: true}},
    {label: 'Manitoba',                  value: {code: 'MB', href: 'manitoba',
                                                 prefixT1: '5015', prefix428: '5007'}},
    {label: 'New Brunswick',             value: {code: 'NB', href: 'new-brunswick',
                                                 prefixT1: '5004'}},
    {label: 'Newfoundland and Labrador', value: {code: 'NL', href: 'newfoundland-labrador',
                                                 prefixT1: '5001'}},
    {label: 'Northwest Territories',     value: {code: 'NT', href: 'northwest-territories',
                                                 prefixT1: '5012'}},
    {label: 'Nova Scotia',               value: {code: 'NS', href: 'nova-scotia',
                                                 prefixT1: '5015'}},
    {label: 'Nunavut',                   value: {code: 'NU', href: 'nunavut',
                                                 prefixT1: '5014'}},
    {label: 'Ontario',                   value: {code: 'ON', href: 'ontario',
                                                 prefixT1: '5006', prefix428: '5006', has479: true}},
    {label: 'PEI',                       value: {code: 'PE', href: 'prince-edward-island',
                                                 prefixT1: '5002'}},
    {label: 'Quebec',                    value: {code: 'QC', href: 'quebec',
                                                 prefixT1: '5005'}},
    {label: 'Saskatchewan',              value: {code: 'SK', href: 'saskatchewan',
                                                 prefixT1: '5015'}},
    {label: 'Yukon',                     value: {code: 'YT', href: 'yukon',
                                                 prefixT1: '5011'}}
];

export default function Uploads() {
    const [province, setProvince] = useState(null);
    const [inputs, setInputs] = useState({});
    const [submitted, setSubmitted] = useState(null);
    const [output, setOutput] = useState(null);
    const [error, setError] = useState("");
    const [t4, setT4] = useState({});
    const [showT4, setShowT4] = useState(false);
    const inputRef = useRef(null);

    function handleUpload(formKey, multi) {
        return (event) => {
            let newInputs = Object.assign({}, inputs);
            if (multi) {
                newInputs[formKey] = event.target.files;
            }
            else {
                newInputs[formKey] = event.target.files[0];
            }
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

    function handleOverlay (event) {
        setShowT4(true);
    }

    function formInput(label, key, multiple) {
        return <dd ref={inputRef}>
            <input type="file" accept=".pdf" multiple={multiple} name={label} onChange={handleUpload(key, multiple)}/>
            {multiple
             ? <span> or &nbsp;
                 <button onClick={handleOverlay}>Enter data</button>
                 <ReactModal isOpen={showT4} onRequestClose={() => setShowT4(false)}>
                 {T4(t4)}
                 </ReactModal>
               </span>
             : ""}
            </dd>;
    }

    function handleSubmit (event) {
        if (province && inputs) {
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
        <a href="/" class="homelink">taxell.ca</a>
        <hr/>
        <h2>Tax form completion</h2>

        <h3>Step 1. <Dropdown className='provinceRoot' menuClassName='provinceMenu' options={provinces} default={province} onChange={setProvince} placeholder="Select your province"/></h3>

        {province && <>
         <h3>Step 2. Download the <em>fillable</em> PDF forms from <a target="_blank" href="https://canada.ca">canada.ca</a></h3>
         <p>or, more precisely, from the <a target="_blank" href={"https://www.canada.ca/en/revenue-agency/services/forms-publications/tax-packages-years/general-income-tax-benefit-package/" + province.value.href + ".html"}>2023 Income tax package</a> page.</p>
         <p>You will need at least the {province.value.prefix428 ? <>federal tax and {province.value.code}428 forms, <tt>{province.value.prefixT1}-r-fill-23e.pdf</tt> and <tt>{province.value.prefix428}-c-fill-23e.pdf</tt></> : <>federal tax form <tt>{province.value.prefixT1}-r-fill-23e.pdf</tt></>}</p>
         <h3>Step 3. Fill in the downloaded {forms}</h3>
         <p>Don't bother with any fields that are calculated from other fields in the same {formsAgain}, that part will be done for you automatically.</p>
         <p>You can leave out your name, SIN, and other private data, since they're not affecting any numbers.</p>
         <h3>Step 4. Upload the filled {formsAgain}:</h3>
         <dl>
         <dt>T1</dt>
         {formInput("T1 PDF", "T1")}
         {province.value.prefix428
          ? <>
          <dt>{province.value.code}428</dt>
          {formInput("428 PDF", "428")}
          </>
          : ""}
         </dl>
         <h4>You can also optionally upload the following forms, if they apply:</h4>
         <dl>
         <dt>T4 slips</dt>
         {formInput("T4 fillable PDFs", "T4", true)}
         {province.value.has479
          ? <>
          <dt>{province.value.code}479 tax credits form</dt>
          {formInput("479 PDF", "479")}
          </>
          : ""}
         <dt>Schedule 6</dt>
         {formInput("Schedule 6 PDF", "Schedule6")}
         <dt>Schedule 7</dt>
         {formInput("Schedule 7 PDF", "Schedule7")}
         <dt>Schedule 8</dt>
         {formInput("Schedule 8 PDF", "Schedule8")}
         <dt>Schedule 9</dt>
         {formInput("Schedule 9 PDF", "Schedule9")}
         <dt>Schedule 11</dt>
         {formInput("Schedule 11 PDF", "Schedule11")}
         </dl>
         <h3>Step 5. <button name="Calculate"
                             disabled={submitted !== false
                                       || !province || !inputs || !inputs["T1"]
                                       || province.value.prefix428 && !inputs["428"]}
                             onClick={handleSubmit}>Calculate</button></h3>
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
