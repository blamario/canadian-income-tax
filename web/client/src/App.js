import { useRef, useState } from 'react';
import Dropdown from 'react-dropdown';
import ReactModal from 'react-modal';
import JSZip from 'jszip';
import 'react-dropdown/style.css';
import '../../static/shared.css';
import '../../static/t4.css';
import T4 from './t4';
import './App.css';

const provinces = [
    {label: 'Alberta',                   value: {code: 'AB', href: 'alberta',
                                                 prefixT1: '5009', prefix428: '5009'}},
    {label: 'British Columbia',          value: {code: 'BC', href: 'british-columbia',
                                                 prefixT1: '5010', prefix428: '5010', has479: true}},
    {label: 'Manitoba',                  value: {code: 'MB', href: 'manitoba',
                                                 prefixT1: '5000', prefix428: '5007'}},
    {label: 'New Brunswick',             value: {code: 'NB', href: 'new-brunswick',
                                                 prefixT1: '5000'}},
    {label: 'Newfoundland and Labrador', value: {code: 'NL', href: 'newfoundland-labrador',
                                                 prefixT1: '5001'}},
    {label: 'Northwest Territories',     value: {code: 'NT', href: 'northwest-territories',
                                                 prefixT1: '5012'}},
    {label: 'Nova Scotia',               value: {code: 'NS', href: 'nova-scotia',
                                                 prefixT1: '5000'}},
    {label: 'Nunavut',                   value: {code: 'NU', href: 'nunavut',
                                                 prefixT1: '5014'}},
    {label: 'Ontario',                   value: {code: 'ON', href: 'ontario',
                                                 prefixT1: '5006', prefix428: '5006', has479: true}},
    {label: 'PEI',                       value: {code: 'PE', href: 'prince-edward-island',
                                                 prefixT1: '5000'}},
    {label: 'Quebec',                    value: {code: 'QC', href: 'quebec',
                                                 prefixT1: '5005'}},
    {label: 'Saskatchewan',              value: {code: 'SK', href: 'saskatchewan',
                                                 prefixT1: '5000'}},
    {label: 'Yukon',                     value: {code: 'YT', href: 'yukon',
                                                 prefixT1: '5011'}}
];

export default function Uploads() {
    const [province, setProvince] = useState(null);
    const [inputs, setInputs] = useState({});
    const [submitted, setSubmitted] = useState(null);
    const [saved, setSaved] = useState(false);
    const [output, setOutput] = useState(null);
    const [error, setError] = useState("");
    const [messages, setMessages] = useState([]);
    const [t4s, setT4s] = useState([]);
    const [showT4, setShowT4] = useState(-1);
    const inputRef = useRef(null);

    function handleLoad(event) {
        JSZip.loadAsync(event.target.files[0]).then(archive => {
            const provinceFile = archive.file('province');
            try {
                let newInputs = {};
                archive.forEach((path, file) => {
                    switch (path) {
                    case 'province':
                        break;
                    case 'T4s':
                        file.async("text").then(contents => setT4s(JSON.parse(contents)));
                        break;
                    default:
                        dir = path.split("/")[0];
                        switch (dir) {
                        case "T1":
                        case "Provincial428":
                        case "Provincial479":
                        case "Schedule6":
                        case "Schedule7":
                        case "Schedule8":
                        case "Schedule9":
                        case "Schedule11":
                            file.async("blob").then(contents => newInputs[path] = contents);
                            break;
                        default:
                            setError("Invalid save file: directory " + dir);
                        }
                    }
                });
                if (provinceFile) {
                    setInputs(newInputs);
                    setSaved(true);
                    setSubmitted(false);
                    provinceFile.async("text").then(code => setProvince(provinces.find(p => p.value.code == code)));
                    setError("");
                }
                else {
                    setError("Invalid save file: no province");
                }
            }
            catch (err) {
                if (err instanceof SyntaxError) {
                    setError("Invalid save file: JSON");
                }
                else {
                    setError("Invalid save file.");
                }
            }
        });
    }

    function saveFile(url, filename) {
        const a = document.createElement("a");
        a.href = url;
        a.download = filename;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
    }

    function handleSave() {
        function handleSaveResponse (response) {
            if (response.ok) {
                response.blob().then(content => saveFile(URL.createObjectURL(new Blob([content],
                                                                                      {type: 'application/zip'})),
                                                         "taxell-save.zip"));
            }
            else {
                response.text().then(setError);
            }
        }
        if (province && inputs) {
            const forms = new FormData();

            if (t4s.length) {
                forms.append("T4", JSON.stringify(t4s));
            }
            for (const key in inputs)
                forms.append(key, inputs[key]);
            fetch("/save/" + province.value.code, {method: "POST", mode: "same-origin", body: forms})
                .then(handleSaveResponse, setError)
                .catch(setError);
        }
        setSaved(true);
    }

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
            setSaved(false);
            setSubmitted(false);
            setError("");
            setOutput(null);
            setMessages([]);
        }
    }

    function handleResponse (response) {
        if (response.ok) {
            const msgsHeader = response.headers.get('X-Tax-Messages');
            setMessages(msgsHeader ? JSON.parse(msgsHeader) : []);
            response.blob().then(setOutput);
        }
        else {
            response.text().then(setError);
        }
    }

    function formInput(label, key, multiple) {
        return <dd ref={inputRef}>
            <input type="file" accept=".pdf" multiple={multiple} name={label} onChange={handleUpload(key, multiple)}/>
            </dd>;
    }

    function slipInput (t4, index) {
        function handleOverlay (event) {
            setShowT4(index);
        }

        function handleDelete (event) {
            setT4s(t4s.slice().splice(index, 1));
        }

        function setT4_ (values) {
            let newT4s = t4s.slice();
            newT4s[index] = values;
            setSaved(false);
            setSubmitted(false);
            setT4s(newT4s);
        }

        return <dd ref={inputRef} key={index}>
                   <button className="btn-secondary enter T4" name="enter" onClick={handleOverlay}>Enter slip #{index+1}</button>
                   <ReactModal isOpen={showT4 === index} onRequestClose={() => setShowT4(-1)}>
                       {T4(t4, setT4_)}
                   </ReactModal>
                   <button className="btn-secondary delete T4" name="delete" onClick={handleDelete}>Delete slip #{index+1}</button>
               </dd>;
    }

    function handleAddT4 (label, key) {
        setT4s(t4s.concat([{}]));
    }

    function handleSubmit (label, key) {
        if (province && inputs) {
            const forms = new FormData();

            if (t4s.length) {
                forms.append("T4", JSON.stringify(t4s));
            }
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
      <div className="app-container">
        <header className="app-header">
          <div className="app-header-title">
            <a href="/" className="homelink">taxell.ca</a>
            <span style={{fontWeight: 600, fontSize: '1.1rem'}}>Tax form completion</span>
          </div>
          <div className="app-header-actions">
            <button className="btn-secondary top save" name="save" disabled={saved} onClick={handleSave}>Save</button>
            <button className="btn-secondary top load" name="load">
                <label htmlFor="load">Load</label>
                <input id="load" type="file" className="load" accept=".zip" name="Load" onChange={handleLoad}/>
            </button>
          </div>
        </header>

        <div className="step">
          <span className="step-badge">1</span>
          <span className="step-label">
            <Dropdown className='provinceRoot' menuClassName='provinceMenu' options={provinces} value={province} onChange={setProvince} placeholder="Select your province or territory"/>
          </span>
        </div>

        {province && <>
         <div className="step">
           <span className="step-badge">2</span>
           <span className="step-label">Download the <em>fillable</em> PDF forms from <a target="_blank" href="https://canada.ca">canada.ca</a></span>
         </div>
         <div className="section-body">
           <p>More precisely, from the <a target="_blank" href={"https://www.canada.ca/en/revenue-agency/services/forms-publications/tax-packages-years/general-income-tax-benefit-package/" + province.value.href + ".html"}>2025 Income tax package</a> page.</p>
           <p>You will need at least the {province.value.prefix428 ? <>federal tax and {province.value.code}428 forms, <tt>{province.value.prefixT1}-r-fill-25e.pdf</tt> and <tt>{province.value.prefix428}-c-fill-25e.pdf</tt></> : <>federal tax form <tt>{province.value.prefixT1}-r-fill-25e.pdf</tt></>}</p>
         </div>

         <div className="step">
           <span className="step-badge">3</span>
           <span className="step-label">Fill in the downloaded {forms}</span>
         </div>
         <div className="section-body">
           <p>Don't bother with any fields that are calculated from other fields in the same {formsAgain}, that part will be done for you automatically.</p>
           <p>You can leave out your name, SIN, and other private data, since they're not affecting any numbers.</p>
         </div>

         <div className="step">
           <span className="step-badge">4</span>
           <span className="step-label">Upload the filled {formsAgain}:</span>
         </div>
         <div className="section-body">
           <dl className="upload-list">
           <dt>T1</dt>
           {formInput("T1 PDF", "T1")}
           {province.value.prefix428
            ? <>
            <dt>{province.value.code}428</dt>
            {formInput("428 PDF", "Provincial428")}
            </>
            : ""}
           </dl>
           <h4>Optional forms (upload if they apply to you):</h4>
           <dl className="upload-list">
           <dt>T4 slips</dt>
           {formInput("T4 fillable PDFs", "T4")}
           <dd>or enter the slips manually:</dd>
           {t4s.map(slipInput)}
           <dd><button className="btn-secondary add T4" name="add" onClick={handleAddT4}>+</button></dd>
           {province.value.has479
            ? <>
            <dt>{province.value.code}479 tax credits form</dt>
            {formInput("479 PDF", "Provincial479")}
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
         </div>

         <div className="step">
           <span className="step-badge">5</span>
           <span className="step-label">
             <button className="btn-primary" name="Calculate"
                             disabled={submitted !== false
                                       || !province || !inputs || !inputs["T1"]
                                       || province.value.prefix428 && !inputs["Provincial428"]}
                             onClick={handleSubmit}>Calculate</button>
           </span>
         </div>

         {output && <>
          <div className="step">
            <span className="step-badge">6</span>
            <span className="step-label">
              <a className="download-link" name="Download" download={download.name}
                   href={URL.createObjectURL(new File([output], download))}>Download {download.name}</a>
            </span>
          </div>
          {messages.length > 0 && <div className="messages section-body">
            <h4>Notes on your return:</h4>
            <ul>
              {messages.map((msg, i) =>
                <li key={i} className={"message " + msg.severity.toLowerCase()}>{msg.text}</li>
              )}
            </ul>
          </div>}
          <div className="step">
            <span className="step-badge">7</span>
            <span className="step-label">Carefully examine the downloaded {forms}. You can also make adjustments and go to Step 4 again.</span>
          </div>
          <div className="step">
            <span className="step-badge">8</span>
            <span className="step-label">Fill in your name, address, SIN, and other private information.</span>
          </div>
          <div className="step">
            <span className="step-badge">9</span>
            <span className="step-label">Print the {formsAgain} and mail your return to CRA.</span>
          </div>
          </>
         }
         </>
        }
        {error && <p className="error-box">{error.toString()}</p>}
      </div>
    );
}
