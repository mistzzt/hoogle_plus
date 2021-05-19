// Ensure that if it's not specified, nothing is limited.
const defaultValue = !(process.env.REACT_APP_LIMIT_FEATURES || false);

const features = {
    search: {
        permitExamples: false,
        permitTypeCandidates: defaultValue,
    },
    results: {
        permitExamples: defaultValue,
        permitEditExamples: false,
        permitKeepUsage: false,
        enableGetMoreExamples: defaultValue,
    }
};

export const getDefaultFeatures = () => {
    return features;
}

export const defaultExamplesShown = 3;
export const defaultExamplesShownIncrement = 2;
