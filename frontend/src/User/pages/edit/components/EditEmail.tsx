import React, { FunctionComponent, useState } from "react";
import EditEmailModal from "./EditEmailModal";

interface EditEmailProps {
  setCurrEmail: React.Dispatch<React.SetStateAction<string | null>>;
  currEmail: string | null;
  emailErr: string | null;
}

const EditEmail: FunctionComponent<EditEmailProps> = ({
  setCurrEmail,
  currEmail,
  emailErr,
}) => {
  const [openEditEmailModal, setOpenEditEmailModal] = useState<boolean>(false); // email edit modal open state

  return (
    <div className="mt-4 w-3/4">
      <label
        className={`block mb-2 text-base font-medium ${
          emailErr ? "text-red-900" : "text-gray-900"
        }`}
      >
        Email
      </label>
      <input
        disabled
        className={`bg-gray-200 appearance-none border-2 ${
          emailErr ? "border-red-200" : "border-gray-200"
        } rounded w-full py-2 px-4 ${
          emailErr ? "text-red-700" : "text-gray-700"
        } leading-tight`}
        type="text"
        placeholder={currEmail || ""}
        value={currEmail || ""}
      />
      <button className="block" onClick={() => setOpenEditEmailModal(true)}>
        Change email
      </button>
      {openEditEmailModal ? (
        <EditEmailModal
          setOpenEditEmailModal={setOpenEditEmailModal}
          setNewEmail={setCurrEmail}
        />
      ) : null}
      {emailErr ? (
        <span className="block text-red-900 text-sm">{emailErr}</span>
      ) : null}
    </div>
  );
};

export default EditEmail;
