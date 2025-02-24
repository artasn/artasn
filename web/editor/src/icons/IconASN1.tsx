import React from 'react';
import { SvgIcon, SvgIconProps } from '@mui/material';

export default function IconASN1(props: SvgIconProps) {
    return (
        <SvgIcon
            {...props}>
            {/* tslint:disable-next-line: max-line-length */}
            <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 500 500">
                <path
                    d="M 240.001 27.848 Q 250.001 22.152 260.001 27.848 L 440.002 130.38 Q 450.002 136.076 450.002 147.468 L 450.002 352.532 Q 450.002 363.924 440.002 369.62 L 260.001 472.152 Q 250.001 477.848 240.001 472.152 L 60 369.62 Q 50 363.924 50 352.532 L 50 147.468 Q 50 136.076 60 130.38 Z"
                    style={{ stroke: 'rgb(0, 0, 0)', fill: 'rgb(210, 185, 255)', strokeWidth: '4px', }} />
            </svg>
        </SvgIcon>
    );
}
