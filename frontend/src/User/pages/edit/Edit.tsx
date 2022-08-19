import React, { FunctionComponent, useState } from "react";
import Alert, { AlertType } from "../../../commonComponents/Alert";
import { API_ROOT } from "../../../constants";
import { useAuth } from "../../../context/authContext";
import { auth_net_patch } from "../../../utils";
import EditAvatar from "./components/EditAvatar";
import EditEmail from "./components/EditEmail";
import EditUsername from "./components/EditUsername";

const usersEndpoint = `${API_ROOT}/users/me`;

interface Payload {
  email?: string;
  username?: string;
  avatar?: string;
}

const Spinner = () => {
  return (
    <div role="status">
      <svg
        aria-hidden="true"
        className="ml-2 w-4 h-4 text-gray-200 animate-spin fill-green-600"
        viewBox="0 0 100 101"
        fill="none"
        xmlns="http://www.w3.org/2000/svg"
      >
        <path
          d="M100 50.5908C100 78.2051 77.6142 100.591 50 100.591C22.3858 100.591 0 78.2051 0 50.5908C0 22.9766 22.3858 0.59082 50 0.59082C77.6142 0.59082 100 22.9766 100 50.5908ZM9.08144 50.5908C9.08144 73.1895 27.4013 91.5094 50 91.5094C72.5987 91.5094 90.9186 73.1895 90.9186 50.5908C90.9186 27.9921 72.5987 9.67226 50 9.67226C27.4013 9.67226 9.08144 27.9921 9.08144 50.5908Z"
          fill="currentColor"
        />
        <path
          d="M93.9676 39.0409C96.393 38.4038 97.8624 35.9116 97.0079 33.5539C95.2932 28.8227 92.871 24.3692 89.8167 20.348C85.8452 15.1192 80.8826 10.7238 75.2124 7.41289C69.5422 4.10194 63.2754 1.94025 56.7698 1.05124C51.7666 0.367541 46.6976 0.446843 41.7345 1.27873C39.2613 1.69328 37.813 4.19778 38.4501 6.62326C39.0873 9.04874 41.5694 10.4717 44.0505 10.1071C47.8511 9.54855 51.7191 9.52689 55.5402 10.0491C60.8642 10.7766 65.9928 12.5457 70.6331 15.2552C75.2735 17.9648 79.3347 21.5619 82.5849 25.841C84.9175 28.9121 86.7997 32.2913 88.1811 35.8758C89.083 38.2158 91.5421 39.6781 93.9676 39.0409Z"
          fill="currentFill"
        />
      </svg>
      <span className="sr-only">Loading...</span>
    </div>
  );
};

const Edit: FunctionComponent = () => {
  const auth = useAuth();
  const [loading, setLoading] = useState<boolean>(false); // save change loading state
  const [currAvatar, setCurrAvatar] = useState<string>(
    auth?.user?.avatar ? auth?.user?.avatar : "/assets/default-pp.webp"
  ); // current avatar state
  const [currUsername, setCurrUsername] = useState<string | null>(
    auth?.user?.username ? auth.user.username : null
  ); // current username state
  const [currEmail, setCurrEmail] = useState<string | null>(
    auth?.user?.email ? auth.user.email : null
  ); // current email satte
  const [usernameErr, setUsernameErr] = useState<string | null>(null); // curr username error state
  const [emailErr, setEmailErr] = useState<string | null>(null); // curr username error state
  const [openAlert, setOpenAlert] = useState<AlertType>({
    type: "",
    isOpen: false,
  }); //open alert box

  return (
    <div className="w-full flex flex-col justify-center items-center" id="myid">
      <EditAvatar currAvatar={currAvatar} setCurrAvatar={setCurrAvatar} />
      <EditUsername
        setCurrUsername={setCurrUsername}
        currUsername={currUsername}
        usernameError={usernameErr}
      />
      <EditEmail
        setCurrEmail={setCurrEmail}
        currEmail={currEmail}
        emailErr={emailErr}
      />

      <button
        type="button"
        disabled={loading}
        onClick={() => {
          setLoading(true);
          //patch and set errors
          const payload: Payload = {};
          if (currEmail) payload["email"] = currEmail;
          if (currAvatar) payload["avatar"] = currAvatar;
          if (currUsername) payload["username"] = currUsername;
          auth_net_patch(usersEndpoint, payload).then((data) => {
            console.log(data);
            if (data.error) {
              if (data.message.includes("username"))
                setUsernameErr(data.message);
              else if (data.message.includes("email"))
                setEmailErr(data.message);
              else {
                console.error("save changes ", data.error);
              }
              setOpenAlert({ type: "error", isOpen: true });
            } else {
              // untoggle errors
              setEmailErr(null);
              setUsernameErr(null);

              //alert feedback
              setOpenAlert({ type: "success", isOpen: true });
              auth?.reset();
            }
            setLoading(false);
          });
        }}
        className={`text-white w-3/4 mt-4 flex justify-center ${
          loading
            ? "bg-green-600"
            : "hover:bg-green-800 focus:ring-4 focus:outline-none focus:ring-green-300 dark:focus:ring-green-800 bg-green-600"
        }  font-medium rounded-lg text-sm inline-flex items-center px-5 py-2.5 text-center mr-2`}
      >
        <div>{loading ? <Spinner /> : "Save changes"}</div>
      </button>
      {openAlert.isOpen ? (
        <Alert
          alert={openAlert}
          setOpenAlert={setOpenAlert}
          message={
            usernameErr || emailErr ? "Changes did not save" : "Changes saved"
          }
        />
      ) : null}
    </div>
  );
};

export default Edit;
