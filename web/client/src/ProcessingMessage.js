import React, { useState, useEffect } from 'react';
import './ProcessingMessage.css';

export function ProcessingMessage({ isProcessing, message, onComplete }) {
    const [displayMessage, setDisplayMessage] = useState('');
    const [isVisible, setIsVisible] = useState(false);

    useEffect(() => {
        if (isProcessing) {
            setIsVisible(true);
            setDisplayMessage(message || 'Processing tax forms...');
        } else if (message && !isProcessing) {
            setDisplayMessage(message);
            // Auto-hide after 5 seconds
            const timer = setTimeout(() => {
                setIsVisible(false);
                if (onComplete) onComplete();
            }, 5000);
            return () => clearTimeout(timer);
        }
    }, [isProcessing, message, onComplete]);

    if (!isVisible) return null;

    return (
        <div className={`processing-message ${isProcessing ? 'processing' : 'complete'}`}> 
            {isProcessing && <div className="spinner"></div>} 
            <p>{displayMessage}</p>
        </div>
    );
}

export default ProcessingMessage;