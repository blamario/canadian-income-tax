import { useState } from 'react';

export default function Uploads() {
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

    return (
        <>
          <h2>T1 form completion</h2>

          <input type="file" name="T1 PDF" onChange={handleUpload}/>
          <button name="Calculate" disabled={submitted !== false} onClick={handleSubmit}>Calculate</button>
            {t1output
             ? <a name="Download" download="t1.pdf" href={console.log(t1output), URL.createObjectURL(new File([t1output], {name: "t1.pdf", type: 'application/PDF'}))}>Download</a>
             : <span>Download</span>}
            <p>{error.toString()}</p>
        </>
    );
}
