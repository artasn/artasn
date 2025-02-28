import React, { Fragment } from 'react';

export function joinNodes(elements: React.ReactNode[], join: React.ReactNode): React.ReactNode {
    return elements.map((item, index) => (
        <Fragment key={index.toString()}>
            {index > 0 && join}
            {item}
        </Fragment>
    ));
}
