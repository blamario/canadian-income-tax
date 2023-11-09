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
            setT1output(response.body);
        }
        else {
            setError(response.body);
        }
    }

    function handleSubmit (event) {
        if (t1input) {
            fetch("/t1/fdf", {"method": "POST", "mode": "same-origin", "body": t1input})
                .then(handleResponse, setError)
                .catch(setError);
            setSubmitted(true);
        }
    }

    return (
        <>
          <h2>T1 form completion</h2>

          <input type="file" name="T1 FDF" onChange={handleUpload}/>
          <button name="Calculate" disabled={submitted !== false} onClick={handleSubmit}>Calculate</button>
            {t1output
             ? <a name="Download" download="t1.fdf" href={console.log(t1output), URL.createObjectURL(new File(t1output, {name: "t1.fdf", type: 'application/FDF'}))}>Download</a>
             : <span>Download</span>}
            <p>{error.toString()}</p>
        </>
    );
}
