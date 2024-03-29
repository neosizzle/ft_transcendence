import React from 'react';
import ReactDOM from 'react-dom/client';
import App from './App';
import './index.css'

const root = ReactDOM.createRoot(
  document.getElementById('root') as HTMLElement
);
// stritchmode is disabled here because of double rerender issue for react router https://stackoverflow.com/questions/52326686/react-router-v4-rendering-component-twice
root.render(
    <App />
);