import React, { FunctionComponent } from "react";

interface EditUserNameProps {
  setCurrUsername: React.Dispatch<React.SetStateAction<string | null>>;
  currUsername: string | null;
  usernameError: string | null;
}

const EditUsername: FunctionComponent<EditUserNameProps> = ({
  setCurrUsername,
  currUsername,
  usernameError,
}) => {
  return (
    <div className="mt-4 w-3/4">
      <label
        className={`block mb-2 text-base font-medium ${
          usernameError ? "text-red-900" : "text-gray-900"
        }`}
      >
        Username
      </label>
      <input
        className={`bg-white appearance-none border-2 ${
          usernameError ? "border-red-200" : "border-gray-200"
        } rounded w-full py-2 px-4 text-gray-700 leading-tight`}
        type="text"
        placeholder={currUsername || ""}
        value={currUsername || ""}
        onChange={(e) => setCurrUsername(e.target.value)}
      />
      {usernameError ? (
        <span className="block text-red-900 text-sm">{usernameError}</span>
      ) : null}
    </div>
  );
};

export default EditUsername;
