% Status (intuit.) : Theorem
% (p => q) | ((p => q) => p)

(((p => r) | ((p => r) => p)) & ((p => s) | ((s => p) => s))) => (p => (r & s)) | ((p => (r & s)) => p)
