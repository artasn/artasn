import React from 'react';
import IconASN1 from '../icons/IconASN1';

import styles from './LoadingSpinner.module.css';

export interface LoadingSpinnerProps {
    size?: number;
}

export default function LoadingSpinner({ size }: LoadingSpinnerProps) {
    const px = `${size ?? 44}px`;

    return (
        <div className={styles.rotating}>
            <IconASN1 sx={{
                width: px,
                height: px,
            }} />
        </div>
    );
}
