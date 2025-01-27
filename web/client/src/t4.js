
export default function T4 (values, setValues) {
    function t4box (number, english, french) {
        return <div class="box">
                 <label class="bilingual-text">{english}<br/>{french}</label>
                 <span class="box-number">{number}</span>
                 <input type="number" step="0.01" value={values[number]} onChange={handleEntry(number)}/>
               </div>;
    }

    function handleEntry (number) {
        return (event) => {
            let newValues = Object.assign({}, values);
            newValues[number] = Number.parseFloat(event.target.value);
            setValues(newValues);
        }
    }
    
    return <div class="grid-container">
        {t4box("14", "Employment income", "Revenus d'emploi")}
        {t4box("22", "Income tax deducted", "Impôt sur le revenu retenu")}
        {t4box("16", "Employee's CPP contributions", "Cotisations de l'employé au RPC")}
        {t4box("17", "Employee's QPP contributions", "Cotisations de l'employé au RRQ")}
        {t4box("16A", "Employee's second CPP contributions – see over",
               "Deuxièmes cotisations de l'employé au RPC – voir au verso")}
        {t4box("17A", "Employee's second QPP contributions – see over",
               "Deuxièmes cotisations de l'employé au RRQ – voir au verso")}
        {t4box("24", "EI insurable earnings", "Gains assurables d'AE")}
        {t4box("26", "CPP/QPP pensionable earnings", "Gains ouvrant droit à pension – RPC/RRQ")}
        {t4box("18", "Employee's EI premiums", "Cotisations de l'employé à l'AE")}
        {t4box("44", "Union dues", "Cotisations syndicales")}
        {t4box("20", "RPP contributions", "Cotisations à un RPA")}
        {t4box("46", "Charitable donations", "Dons de bienfaisance")}
        {t4box("52", "Pension adjustment", "Facteur d'équivalence")}
        {t4box("50", "RPP or DPSP registration number", "N° d'agrément d'un RPA ou d'un RPDB")}
        {t4box("55", "Employee's PPIP premiums – see over", "Cotisations de l'employé au RPAP – voir au verso")}
        {t4box("56", "PPIP insurable earnings", "Gains assurables du RPAP")}
        {t4box("45", "Employer-offered dental benefits", "Prestations dentaires offertes par l'employeur")}
    </div>;
}
