import React, { StrictMode } from "react";
import { createRoot } from "react-dom/client";
import "./styles.css";

import Uploads from "./App";

const root = createRoot(document.getElementById("root"));
root.render(
  <StrictMode>
    <Uploads />
  </StrictMode>
);
